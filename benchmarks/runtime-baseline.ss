;;; Runtime baseline for comparing builds with identical compiler settings.
;;;
;;; Usage:
;;;   scheme --script benchmarks/runtime-baseline.ss [ITERATIONS [FOREIGN1.SO]]

(import (chezscheme))

(define nanoseconds-per-second 1000000000)

(define (time->nanoseconds t)
  (+ (* (time-second t) nanoseconds-per-second)
     (time-nanosecond t)))

(define (measure name iterations thunk)
  (collect)
  (let* ([start (time->nanoseconds (current-time 'time-monotonic))]
         [result (thunk)]
         [stop (time->nanoseconds (current-time 'time-monotonic))]
         [elapsed (- stop start)])
    (pretty-print
      `(,name
         (iterations ,iterations)
         (result ,result)
         (nanoseconds ,elapsed)
         (nanoseconds-per-iteration ,(div elapsed iterations))))))

(define args (cdr (command-line)))
(define iterations
  (if (pair? args)
      (let ([value (string->number (car args))])
        (unless (and (integer? value) (exact? value) (positive? value))
          (error 'runtime-baseline "invalid iteration count" value))
        value)
      10000000))
(define foreign-object
  (and (pair? args) (pair? (cdr args)) (cadr args)))

(measure 'tight-loop iterations
  (lambda ()
    (let loop ([i iterations] [value 0])
      (if (= i 0)
          value
          (loop (- i 1) (fxand (fx+ value 1) #x3fffffff))))))

(let ([increment (lambda (value) (fx+ value 1))])
  (measure 'scheme-call iterations
    (lambda ()
      (let loop ([i iterations] [value 0])
        (if (= i 0)
            value
            (loop (- i 1) (increment value)))))))

(when foreign-object
  (load-shared-object foreign-object)
  (let* ([call-callback
          (foreign-procedure "call_for_interrupt_test" (void* int) int)]
         [callback
          (foreign-callable (lambda (value) (fx+ value 1)) (int) int)]
         [entry (foreign-callable-entry-point callback)]
         [callback-iterations (max 1 (div iterations 100))])
    (do ([i 0 (fx+ i 1)])
        ((fx= i (min callback-iterations 10000)))
      (call-callback entry 0))
    (measure 'foreign-callback callback-iterations
      (lambda ()
        (let loop ([i callback-iterations] [value 0])
          (if (= i 0)
              value
              (loop (- i 1) (call-callback entry value))))))
    (keep-live callback)))
