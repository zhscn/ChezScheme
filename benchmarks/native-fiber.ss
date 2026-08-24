;;; Native-fiber microbenchmarks.
;;;
;;; Usage:
;;;   scheme --script benchmarks/native-fiber.ss
;;;   scheme --script benchmarks/native-fiber.ss SWITCHES PARKED PREEMPTIONS BASELINE

(import (chezscheme))

(define nanoseconds-per-second 1000000000)

(define (time->nanoseconds t)
  (+ (* (time-second t) nanoseconds-per-second)
     (time-nanosecond t)))

(define (measure thunk)
  (let* ([start (time->nanoseconds (current-time 'time-monotonic))]
         [value (thunk)]
         [stop (time->nanoseconds (current-time 'time-monotonic))])
    (values (- stop start) value)))

(define (argument index default)
  (let ([args (cdr (command-line))])
    (if (< index (length args))
        (let ([value (string->number (list-ref args index))])
          (unless (and (integer? value) (exact? value) (positive? value))
            (error 'native-fiber-benchmark "invalid positive integer argument" value))
          value)
        default)))

(define switch-count (argument 0 20000))
(define parked-count (argument 1 256))
(define preemption-count (argument 2 1000))
(define baseline-count (argument 3 5000000))

(define scheduler #f)

(define (at-depth depth thunk)
  (if (= depth 0)
      (thunk)
      (let ([value (at-depth (- depth 1) thunk)])
        ;; Keep this recursion non-tail so the requested stack depth remains
        ;; live across every suspension.
        (if value value #f))))

(define (make-finisher)
  (lambda (fiber outcome)
    (unless (#3%$native-fiber-try-claim! scheduler)
      (error 'native-fiber-benchmark "cannot claim scheduler for finish"))
    (#3%$native-fiber-finish fiber scheduler outcome)))

(define (run-voluntary depth count observe-cache?)
  (let ([task #f]
        [cache #f]
        [cache-stable? #t])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (at-depth depth
            (lambda ()
              (let loop ([i count])
                (unless (= i 0)
                  (unless (#3%$native-fiber-try-claim! scheduler)
                    (error 'native-fiber-benchmark "cannot claim scheduler"))
                  (#3%$native-fiber-switch task scheduler i)
                  (loop (- i 1))))
              'done)))
        (make-finisher)
        #b001))
    (let-values ([(elapsed transfers)
                  (measure
                    (lambda ()
                      (let loop ([transfers 0])
                        (unless (#3%$native-fiber-try-claim! task)
                          (error 'native-fiber-benchmark "cannot claim task"))
                        (begin
                          (#3%$native-fiber-switch scheduler task 'resume)
                          (when observe-cache?
                            (let ([current-cache
                                   (#3%$tc-field 'cached-frame (#3%$tc))])
                              (if cache
                                  (set! cache-stable?
                                    (and cache-stable?
                                         (eq? cache current-cache)))
                                  (set! cache current-cache))))
                          (if (eq? (#3%$native-fiber-state task) 'finished)
                              (+ transfers 2)
                              (loop (+ transfers 2)))))))])
      (values elapsed transfers (and cache cache-stable?)))))

(define (report-voluntary-depth depth)
  (collect)
  (let-values ([(elapsed transfers cache-reused?)
                (run-voluntary depth switch-count #t)])
    (pretty-print
      `(voluntary-switch
         (depth ,depth)
         (transfers ,transfers)
         (nanoseconds ,elapsed)
         (nanoseconds-per-transfer ,(div elapsed transfers))
         (descriptor-cache-reused? ,cache-reused?)))))

(define (make-parked-fibers depth count)
  (let loop ([i count] [fibers '()])
    (if (= i 0)
        fibers
        (let ([task #f])
          (set! task
            (#3%$native-fiber-create
              (lambda ()
                (at-depth depth
                  (lambda ()
                    (unless (#3%$native-fiber-try-claim! scheduler)
                      (error 'native-fiber-benchmark "cannot claim scheduler"))
                    (#3%$native-fiber-switch task scheduler 'parked)
                    'done)))
              (make-finisher)
              #b001))
          (unless (#3%$native-fiber-try-claim! task)
            (error 'native-fiber-benchmark "cannot claim new task"))
          (#3%$native-fiber-switch scheduler task 'start)
          (loop (- i 1) (cons task fibers))))))

(define (finish-parked-fibers fibers)
  (for-each
    (lambda (task)
      (unless (#3%$native-fiber-try-claim! task)
        (error 'native-fiber-benchmark "cannot claim parked task"))
      (#3%$native-fiber-switch scheduler task 'finish)
      (unless (eq? (#3%$native-fiber-state task) 'finished)
        (error 'native-fiber-benchmark "task did not finish")))
    fibers))

(define (report-parked-collection depth)
  (collect)
  (let ([fibers (make-parked-fibers depth parked-count)])
    (let-values ([(elapsed ignored)
                  (measure
                    (lambda ()
                      (collect (collect-maximum-generation))))])
      (pretty-print
        `(parked-fiber-collection
           (depth ,depth)
           (fibers ,parked-count)
           (nanoseconds ,elapsed)
           (nanoseconds-per-fiber ,(div elapsed parked-count)))))
    (finish-parked-fibers fibers)))

(define (run-preemptive count)
  (let ([task #f]
        [preemptions 0]
        [saved-handler (timer-interrupt-handler)])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (set-timer 1000)
          (let loop ([value 0])
            (if (= preemptions count)
                value
                (loop (fxand (fx+ value 1) #x3fffffff)))))
        (make-finisher)
        #b001))
    (dynamic-wind
      (lambda ()
        (timer-interrupt-handler
          (lambda ()
            (let ([transfer
                   (#3%$native-fiber-preempt scheduler 'preempted)])
              (if transfer
                  (set! preemptions (+ preemptions 1))
                  (set-timer 1000))
              transfer))))
      (lambda ()
        (let-values ([(elapsed transfers)
                      (measure
                        (lambda ()
                          (let loop ([transfers 0])
                            (unless (#3%$native-fiber-try-claim! task)
                              (error 'native-fiber-benchmark
                                "cannot claim preempted task"))
                            (begin
                              (#3%$native-fiber-switch scheduler task 'resume)
                              (if (eq? (#3%$native-fiber-state task) 'finished)
                                  (+ transfers 2)
                                  (begin
                                    ;; Arm the next quantum from the scheduler,
                                    ;; after the preceding handler has unwound.
                                    (set-timer 1000)
                                    (loop (+ transfers 2))))))))])
          (values elapsed transfers preemptions)))
      (lambda ()
        (set-timer 0)
        (timer-interrupt-handler saved-handler)))))

(define (report-preemptive)
  (collect)
  (let-values ([(elapsed transfers preemptions)
                (run-preemptive preemption-count)])
    (unless (and (= preemptions preemption-count)
                 (= transfers (* 2 (+ preemptions 1))))
      (error 'native-fiber-benchmark
        "preemption accounting mismatch" preemptions transfers))
    (pretty-print
      `(preemptive-switch
         (preemptions ,preemptions)
         (transfers ,transfers)
         (nanoseconds ,elapsed)
         (nanoseconds-per-preemption ,(div elapsed preemptions))))))

(define (report-baseline)
  (collect)
  (let-values ([(elapsed result)
                (measure
                  (lambda ()
                    (let loop ([i baseline-count] [value 0])
                      (if (= i 0)
                          value
                          (loop (- i 1)
                            (fxand (fx+ value 1) #x3fffffff))))))])
    (pretty-print
      `(non-fiber-baseline
         (iterations ,baseline-count)
         (result ,result)
         (nanoseconds ,elapsed)
         (nanoseconds-per-iteration ,(div elapsed baseline-count))))))

(pretty-print
  `(native-fiber-benchmark
     (machine ,(machine-type))
     (switches ,switch-count)
     (parked ,parked-count)
     (preemptions ,preemption-count)
     (baseline ,baseline-count)))

(report-baseline)
(set! scheduler
  (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0)))
(for-each report-voluntary-depth '(0 16 64 256 1024))
(report-parked-collection 0)
(report-parked-collection 256)
(report-preemptive)
