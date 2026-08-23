;;; Concurrent stress coverage for the async scheduler and libuv integration.

(import (chezscheme))
(import (chezscheme async))
(import (chezscheme async operations))
(import (chezscheme async channels))
(import (chezscheme async context))
(import (chezscheme async sync))
(import (chezscheme async io fs))

(define environment-positive-integer
  (lambda (name default)
    (let ([v (getenv name)])
      (if v
          (let ([n (string->number v)])
            (unless (and (fixnum? n) (fx> n 0))
              (error 'async-stress "invalid positive integer environment variable" name v))
            n)
          default))))

(define atomic-box-ref
  (lambda (b)
    (let loop ()
      (let ([v (unbox b)])
        (if (box-cas! b v v) v (loop))))))

(define atomic-box-set!
  (lambda (b new)
    (let loop ()
      (let ([old (atomic-box-ref b)])
        (unless (box-cas! b old new) (loop))))))

(define stress-iterations
  (environment-positive-integer "CHEZ_ASYNC_STRESS_ITERATIONS" 100))
(define stress-timeout
  (environment-positive-integer "CHEZ_ASYNC_STRESS_TIMEOUT" 120))
(define stress-io?
  (let ([v (getenv "CHEZ_ASYNC_STRESS_IO")])
    (not (and v (member v '("" "0" "false" "no"))))))
(define stress-scenario (or (getenv "CHEZ_ASYNC_STRESS_SCENARIO") "all"))
(define stress-trace? (and (getenv "CHEZ_ASYNC_STRESS_TRACE") #t))
(define stress-scenario?
  (lambda (name)
    (or (string=? stress-scenario "all")
        (string=? stress-scenario name)
        (and (string=? stress-scenario "parameter-io")
             (or (string=? name "parameter")
                 (string=? name "io"))))))
(define stress-done? (box #f))
(define stress-location (box '(startup)))

(define watchdog
  (fork-thread
    (lambda ()
      (let loop ([remaining stress-timeout])
        (cond
          [(atomic-box-ref stress-done?) (void)]
          [(fx= remaining 0)
           (fprintf (console-error-port)
             "async stress timed out after ~s seconds at ~s\n"
             stress-timeout (atomic-box-ref stress-location))
           (flush-output-port (console-error-port))]
          [else
           (sleep (make-time 'time-duration 0 1))
           (loop (fx- remaining 1))])))))

(define finish-watchdog!
  (lambda ()
    (box-cas! stress-done? #f #t)))

(define check
  (lambda (ok? name iteration value)
    (unless ok?
      (error 'async-stress "stress check failed" name iteration value))))

(define stress-mark!
  (lambda (location)
    (atomic-box-set! stress-location location)
    (when stress-trace?
      (printf "async stress: ~s\n" location)
      (flush-output-port (current-output-port)))))

(define stress-parameter-publication
  (lambda (iteration)
    (let ([value
           (run-async
             (lambda ()
               (let ([ready (make-future)]
                     [parameter-box (box #f)]
                     [readers (make-vector 8)])
                 (do ([i 0 (fx+ i 1)]) ((fx= i (vector-length readers)))
                   (vector-set! readers i
                     (spawn-task
                       (lambda ()
                         (future-get ready)
                         ((unbox parameter-box)))
                       'migratable? #t)))
                 (let ([creator
                        (spawn-task
                          (lambda ()
                            (let ([p (make-thread-parameter 'initial)])
                              (set-box! parameter-box p)
                              (p 'creator)
                              (future-fulfil! ready #t)
                              (task-yield)
                              (p)))
                          'migratable? #t)])
                   (cons (task-join creator)
                     (vector->list (vector-map task-join readers))))))
             'parallelism 4)])
      (check (equal? value (cons 'creator (make-list 8 'initial)))
        'parameter-publication iteration value))))

(define stress-completion-cancellation
  (lambda (iteration)
    (let ([value
           (run-async
             (lambda ()
               (let* ([started (make-future)]
                      [blocked (make-future)]
                      [task
                       (spawn-task
                         (lambda ()
                           (future-fulfil! started #t)
                           (future-get blocked)
                           'completed)
                         'migratable? #t)])
                 (future-get started)
                 (let ([fulfiller
                        (fork-thread
                          (lambda () (future-fulfil! blocked #t)))]
                       [canceler
                        (fork-thread
                          (lambda () (task-cancel! task 'stress-cancel)))])
                   (thread-join fulfiller)
                   (thread-join canceler))
                 (guard (c
                          [(async-cancellation-condition? c) 'canceled]
                          [else (raise c)])
                   (task-join task))))
             'parallelism 4)])
      (check (memq value '(completed canceled))
        'completion-cancellation iteration value))))

(define stress-channel-rendezvous
  (lambda (iteration)
    (let ([value
           (run-async
             (lambda ()
               (let ([channel (make-channel)])
                 (let ([consumer
                        (spawn-task
                          (lambda () (channel-get channel))
                          'migratable? #t)]
                       [producer
                        (spawn-task
                          (lambda () (channel-put channel iteration))
                          'migratable? #t)])
                   (let ([v (task-join consumer)])
                     (task-join producer)
                     v))))
             'parallelism 4)])
      (check (fx= value iteration) 'channel-rendezvous iteration value))))

(define stress-async-mutex
  (lambda (iteration)
    (let ([value
           (run-async
             (lambda ()
               (let ([mutex (make-async-mutex)] [counter 0] [tasks '()])
                 (do ([i 0 (fx+ i 1)]) ((fx= i 8))
                   (set! tasks
                     (cons
                       (spawn-task
                         (lambda ()
                           (do ([j 0 (fx+ j 1)]) ((fx= j 25))
                             (call-with-async-mutex mutex
                               (lambda ()
                                 (let ([old counter])
                                   (task-yield)
                                   (set! counter (fx+ old 1)))))))
                         'migratable? #t)
                       tasks)))
                 (for-each task-join tasks)
                 counter))
             'parallelism 4)])
      (check (fx= value 200) 'async-mutex iteration value))))

(define stress-sync-primitives
  (lambda (iteration)
    (let ([value
           (run-async
             (lambda ()
               (let ([rw-mutex (make-async-rw-mutex)]
                     [once (make-async-once)]
                     [workers-done (make-async-wait-group)]
                     [once-count 0]
                     [counter 0]
                     [scoped-finished? #f]
                     [workers '()])
                 (do ([i 0 (fx+ i 1)]) ((fx= i 12))
                   (set! workers
                     (cons
                       (spawn-task/async-wait-group workers-done
                         (lambda ()
                           (async-once-run! once
                             (lambda ()
                               (task-yield)
                               (set! once-count (fx+ once-count 1))))
                           (call-with-async-rw-mutex rw-mutex
                             (lambda ()
                               (let ([old counter])
                                 (task-yield)
                                 (set! counter (fx+ old 1)))))
                           (call-with-async-read-mutex rw-mutex
                             (lambda () counter)))
                         'migratable? #t)
                       workers)))
                 (async-wait-group-wait workers-done)
                 (for-each task-join workers)
                 (spawn-task/async-wait-group workers-done
                   (lambda ()
                     (task-join
                       (spawn-task (lambda () (void)) 'migratable? #t))
                     (async-sleep 0.001)
                     (set! scoped-finished? #t))
                   'migratable? #t)
                 (async-wait-group-wait workers-done)
                 (let ([canceled
                        (spawn-task/async-wait-group workers-done
                          (lambda () (set! scoped-finished? 'unexpected))
                          'migratable? #t)])
                   (task-cancel! canceled 'before-start)
                   (guard (c [else (void)]) (task-join canceled))
                   (async-wait-group-wait workers-done))
                 (let* ([mutex (make-async-mutex)]
                        [condition (make-async-condition mutex)]
                        [waiters-ready (make-async-wait-group 4)]
                        [ready? #f]
                        [waiters
                         (map
                           (lambda (id)
                             (spawn-task
                               (lambda ()
                                 (call-with-async-mutex mutex
                                   (lambda ()
                                     (async-wait-group-done! waiters-ready)
                                     (let loop ()
                                       (unless ready?
                                         (async-condition-wait condition)
                                         (loop)))
                                     id)))
                               'migratable? #t))
                           '(a b c d))])
                   (async-wait-group-wait waiters-ready)
                   (call-with-async-mutex mutex
                     (lambda ()
                       (set! ready? #t)
                       (async-condition-broadcast! condition)))
                   (list once-count counter scoped-finished?
                     (map task-join waiters)))))
             'parallelism 4)])
      (check (equal? value '(1 12 #t (a b c d)))
        'sync-primitives iteration value))))

(define stress-channel-close-race
  (lambda (iteration)
    (let ([value
           (run-async
             (lambda ()
               (let* ([channel (make-channel)]
                      [producer
                       (spawn-task
                         (lambda ()
                           (guard (c [(channel-closed-condition? c) 'closed])
                             (channel-put channel iteration)
                             'sent))
                         'migratable? #t)]
                      [consumer
                       (spawn-task
                         (lambda ()
                           (call-with-values
                             (lambda () (channel-receive channel)) list))
                         'migratable? #t)]
                      [closer
                       (fork-thread
                         (lambda () (channel-close! channel 'stress-close)))])
                 (thread-join closer)
                 (list (task-join producer) (task-join consumer))))
             'parallelism 4)])
      (check (or (equal? value (list 'sent (list iteration #t)))
                 (equal? value '(closed (#f #f))))
        'channel-close-race iteration value))))

(define stress-context-completion-cancellation
  (lambda (iteration)
    (let ([value
           (run-async
             (lambda ()
               (let* ([context (make-async-context)]
                      [started (make-future)]
                      [blocked (make-future)]
                      [task
                       (spawn-task
                         (lambda ()
                           (future-fulfil! started)
                           (future-get blocked)
                           'completed)
                         'context context
                         'migratable? #t)])
                 (future-get started)
                 (let ([fulfiller
                        (fork-thread
                          (lambda () (future-fulfil! blocked #t)))]
                       [canceler
                        (fork-thread
                          (lambda ()
                            (async-context-cancel! context 'context-stop)))])
                   (thread-join fulfiller)
                   (thread-join canceler))
                 (guard (c [(async-cancellation-condition? c) 'canceled])
                   (task-join task))))
             'parallelism 4)])
      (check (memq value '(completed canceled))
        'context-completion-cancellation iteration value))))

(define stress-file-owner-routing
  (lambda (iteration)
    (stress-mark! (list iteration 'file-owner-routing 'setup))
    (let ([path
           (format "/tmp/chez-async-stress-~a-~a"
             (get-process-id) iteration)])
      (when (file-exists? path) (delete-file path))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([value
                 (run-async
                   (lambda ()
                     (stress-mark! (list iteration 'file-owner-routing 'open))
                     (let* ([file (file-open path '(write create truncate))]
                            [mutex (make-mutex)]
                            [condition (make-condition)]
                            [started? #f]
                            [writer
                             (spawn-task
                               (lambda ()
                                 (stress-mark!
                                   (list iteration 'file-owner-routing
                                     'writer-start))
                                 (with-mutex mutex
                                   (set! started? #t)
                                   (condition-broadcast condition))
                                 (file-write file (string->utf8 "stress"))
                                 (stress-mark!
                                   (list iteration 'file-owner-routing
                                     'writer-written))
                                 (file-close file)
                                 (stress-mark!
                                   (list iteration 'file-owner-routing
                                     'writer-closed))
                                 #t)
                               'migratable? #t)]
                            [nudge
                             (spawn-task (lambda () (void)) 'migratable? #t)])
                       (with-mutex mutex
                         (let loop ()
                           (unless started?
                             (condition-wait condition mutex)
                             (loop))))
                       (stress-mark!
                         (list iteration 'file-owner-routing 'joining))
                       (let ([written? (task-join writer)])
                         (task-join nudge)
                         written?)))
                   'parallelism 2)])
            (stress-mark! (list iteration 'file-owner-routing 'verify))
            (check value 'file-owner-routing iteration value)
            (let ([size (call-with-input-file path file-length)])
              (check (= size 6) 'file-owner-size iteration size))))
        (lambda ()
          (when (file-exists? path) (delete-file path)))))))

(guard (c
         [else
          (finish-watchdog!)
          (fprintf (console-error-port) "async stress failed: ~a\n" c)
          (flush-output-port (console-error-port))
          (exit 1)])
  (do ([i 0 (fx+ i 1)]) ((fx= i stress-iterations))
    (when stress-trace?
      (printf "async stress: starting iteration ~s\n" i)
      (flush-output-port (current-output-port)))
    (when (stress-scenario? "parameter")
      (atomic-box-set! stress-location (list i 'parameter-publication))
      (stress-parameter-publication i))
    (when (stress-scenario? "cancellation")
      (atomic-box-set! stress-location (list i 'completion-cancellation))
      (stress-completion-cancellation i)
      (atomic-box-set! stress-location
        (list i 'context-completion-cancellation))
      (stress-context-completion-cancellation i))
    (when (stress-scenario? "channel")
      (atomic-box-set! stress-location (list i 'channel-rendezvous))
      (stress-channel-rendezvous i)
      (atomic-box-set! stress-location (list i 'channel-close-race))
      (stress-channel-close-race i))
    (when (stress-scenario? "mutex")
      (atomic-box-set! stress-location (list i 'async-mutex))
      (stress-async-mutex i))
    (when (stress-scenario? "sync")
      (atomic-box-set! stress-location (list i 'sync-primitives))
      (stress-sync-primitives i))
    (when (and stress-io? (stress-scenario? "io"))
      (atomic-box-set! stress-location (list i 'file-owner-routing))
      (stress-file-owner-routing i))
    (when (fx= (fxmod (fx+ i 1) 100) 0)
      (printf "async stress: ~s iterations\n" (fx+ i 1))
      (flush-output-port (current-output-port))))
  (finish-watchdog!)
  (thread-join watchdog)
  (printf "async stress completed: ~s iterations, scenario=~a, io=~s\n"
    stress-iterations stress-scenario stress-io?))
