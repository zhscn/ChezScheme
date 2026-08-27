;;; ------------------------------------------------------------------- tasks

(define async-make-task
  (lambda (sched name parent-group parent-context migratable?
           termination-actions entry)
    (let* ([group ($async-scheduler-group sched)]
           [id (async-next-task-id! group)]
           [task #f])
      (set! task
        (make-async-task id name 'ready entry #f group sched #f migratable? #f #f
          parent-group (async-make-context parent-context) #f
          #f                         ; child group
          '()                        ; result values
          #f                         ; failure condition
          (make-async-wait-queue)    ; join waiters
          #f                         ; observed?
          (box #f)                   ; cancel state
          #f                         ; cancel condition
          #f                         ; current wait
          #f                         ; nack thunk
          #f                         ; payload
          #f                         ; sync state
          (box #f)                   ; cancellation shield
          termination-actions '()
          0 0 0 0 0 #f #f (async-monotonic-us)
          (make-async-os-mutex)))
      (async-task-native-fiber-set! task
        ($native-fiber-create
          (lambda ()
            (let ([entry (async-task-entry task)])
              (async-task-entry-set! task #f)
              (async-check-cancellation! task)
              (entry)))
          (lambda (fiber native-outcome)
            (let* ([sched ($async-scheduler)]
                   [scheduler-fiber
                    (and (async-scheduler? sched)
                         (async-scheduler-native-fiber sched))]
                   [outcome
                    (case (car native-outcome)
                      [(return) (cdr native-outcome)]
                      [(exception) (cons 'failed (cdr native-outcome))]
                      [else
                       (cons 'failed
                         (condition
                           (make-error)
                           (make-message-condition
                             "invalid native-fiber task outcome")))])])
              (unless (and scheduler-fiber
                           (eq? task (async-scheduler-current-task sched))
                           ($native-fiber-try-claim! scheduler-fiber))
                ($oops 'async-task "scheduler fiber is not claimable at task exit"))
              ($native-fiber-finish fiber scheduler-fiber outcome)))
          (fxlogor
            (if migratable?
                (constant native-fiber-flag-migratable)
                (constant native-fiber-flag-pinned))
            (if async-debug-invariants?
                (constant native-fiber-flag-debug)
                0))))
      (async-trace-event! group sched task 'create
        (if migratable? 'migratable 'pinned))
      task)))

(define ensure-child-group
  (lambda (task)
    (or (async-task-child-group task)
        (let ([grp (make-async-group (async-task-parent-group task)
                     (async-task-context task))])
          (async-task-child-group-set! task grp)
          grp))))


;;; The body wrapper: run the thunk, then drain the child group.
(define run-task-entry
  (lambda (thunk)
    (let ([outcome
            (guard (c [else (cons 'failed c)])
              (call-with-values thunk (lambda vals (cons 'done vals))))])
      (let* ([sched ($async-scheduler)]
             [grp (async-task-child-group (async-scheduler-current-task sched))])
        (if grp
            (drain-task-group sched grp outcome)
            outcome)))))

;;; Wait for children; report unobserved failures by failing the owning task.
;;; The drain is shielded: the task's own cancellation does not interrupt the
;;; wait for canceled children's cleanup.
(define drain-task-group
  (lambda (sched grp outcome)
    (when (eq? (car outcome) 'failed)
      (group-cancel-children! grp (cdr outcome)))
    (let ([task (async-scheduler-current-task sched)])
      (async-task-cancel-shield?-set! task #t)
      (let loop ()
        (cond
          [(with-async-mutex (async-task-group-mutex grp)
             (async-group-check-locked! grp)
             (async-group-empty?/locked grp))
           (async-task-cancel-shield?-set! task #f)
           (cond
             [(and (eq? (car outcome) 'done)
                   (task-cancel-requested? task))
              (cons 'failed (task-cancellation-condition task))]
             [(eq? (car outcome) 'done)
              (let find ([us
                          (with-async-mutex (async-task-group-mutex grp)
                            (let ([us (async-task-group-unobserved grp)])
                              (async-task-group-unobserved-set! grp '())
                              us))])
                (cond
                  [(null? us) outcome]
                  [(async-task-observe-failure! (caar us))
                   (cons 'failed (cdar us))]
                  [else (find (cdr us))]))]
             [else outcome])]
          [else
           (perform-operation (group-empty-operation grp))
           (loop)])))))

(define terminate-task!
  (lambda (sched task state outcome)
    ;; A task can be canceled after an acquisition has won but before it
    ;; observes the result. Termination is the final ownership
    ;; backstop for both that race and unscoped acquisitions.
    (let ([owned-locks
           (with-async-mutex (async-task-mutex task)
             (let ([locks (async-task-owned-mutex-list task)])
               (async-task-owned-mutexes-set! task '())
               locks))])
      (for-each
        (lambda (lock) (async-release-task-lock! lock task))
        owned-locks))
    ;; Completion actions are installed before task publication, so they also
    ;; run for a task canceled before its entry thunk starts. Lock cleanup
    ;; precedes wakeup of completion observers.
    (let ([actions (async-task-termination-actions task)])
      (async-task-termination-actions-set! task '())
      (for-each (lambda (action) (action)) actions))
    (let ([join-waiters
           (with-async-mutex (async-task-mutex task)
             (async-task-state-set! task state)
             (async-task-current-wait-set! task #f)
             (async-task-wait-scheduler-set! task #f)
             (async-task-suspension-state-set! task #f)
             (case state
               [(completed) (async-task-result-values-set! task (cdr outcome))]
               [else (async-task-failure-condition-set! task (cdr outcome))])
             (let ([waiters
                    (async-wait-queue-drain!
                      (async-task-join-waiters task))])
               (when (and (eq? state 'failed) (pair? waiters))
                 (async-task-observed?-set! task #t))
               waiters))])
      (async-trace-event! ($async-scheduler-group sched) sched task
        'finish state)
      (sched-registry-remove! sched task)
      (let ([join-payload
              (case state
                [(completed) (cons 'values (cdr outcome))]
                [else (cons 'raise (cdr outcome))])])
        (for-each (lambda (w) ((cdr w) join-payload)) join-waiters))
      (async-context-cancel-core! (async-task-context task)
        (case state
          [(completed) 'task-completed]
          [else (cdr outcome)]))
      (when (async-task-parent-group task)
        (group-child-terminated! (async-task-parent-group task) task))
      (let ([group ($async-scheduler-group sched)])
        (when (eq? task (async-scheduler-group-root-task group))
          (async-group-shutdown! group)))
      (void))))

;;; ------------------------------------------------------------------- join

(define task-join-payload
  (lambda (task)
    (case (async-task-state task)
      [(completed) (cons 'values (async-task-result-values task))]
      [else (cons 'raise (async-task-failure-condition task))])))

(define async-task-join-ready-payload!
  (lambda (task)
    (with-async-mutex (async-task-mutex task)
      (and (task-terminal? task)
           (begin
             (when (eq? (async-task-state task) 'failed)
               (async-task-observed?-set! task #t))
             (task-join-payload task))))))

(define async-task-join-operation
  (lambda (task)
    (let ([token (list 'task-join-operation)])
      (make-async-operation
      (lambda (ss)
        (let ([sched ($async-scheduler)])
          (when (and (async-scheduler? sched)
                     (eq? task (async-scheduler-current-task sched)))
            ($oops 'task-join-operation "a task cannot join itself")))
        (async-task-join-ready-payload! task))
      (lambda (ss deliver)
        (let ([payload
               (with-async-mutex (async-task-mutex task)
                 (if (task-terminal? task)
                     (begin
                       (when (eq? (async-task-state task) 'failed)
                         (async-task-observed?-set! task #t))
                       (task-join-payload task))
                     (begin
                       (async-sync-slot-set! ss token
                         (async-wait-queue-add!
                           (async-task-join-waiters task)
                           (cons ss deliver)))
                       #f)))])
          (if payload
              (begin (deliver payload) #f)
              (list 'join (async-task-id task)))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-task-mutex task)
          (let ([node (async-sync-slot-ref ss token #f)])
            (when node
              (async-wait-queue-remove! (async-task-join-waiters task) node)
              (async-sync-slot-delete! ss token)))))))))


;;; ------------------------------------------------------------- cancellation


;;; ------------------------------------------------------------------- yield


;;; --------------------------------------------------------- async dynamic-wind


;;; --------------------------------------------------------------- scheduler

(define async-make-scheduler
  (lambda (virtual? group index preemption-ticks)
    (make-async-scheduler%
      #f group index
      (make-async-queue) (make-async-queue)
      (make-async-work-deque)
      (make-async-queue) (make-async-os-mutex)
      (if-feature pthreads (make-condition) #f)
      #f #f #f (fx+ index 1) #f 'created virtual? 0
      (make-async-timer-heap) #f
      0 0 preemption-ticks 0 0 (box 0) #f #f #f)))

(define async-drain-remote!
  (lambda (sched)
    (with-async-mutex (async-scheduler-remote-mutex sched)
      (let loop ()
        (unless (async-queue-empty? (async-scheduler-remote-queue sched))
          (let ([task (async-queue-pop! (async-scheduler-remote-queue sched))])
            (async-debug-queue-release! task)
            (when (eq? (async-task-state task) 'ready)
              (async-debug-queue-claim! task 'next)
              (async-queue-push! (async-scheduler-next-queue sched) task))
            (loop)))))))

(define async-poll-ready-to-block?
  (lambda (sched)
    (let ([group ($async-scheduler-group sched)])
      ;; Keep the order consistent with the condition-wait path.  A producer
      ;; publishes group work before taking the remote mutex to wake a worker,
      ;; so the worker must take the remote mutex before inspecting group work.
      (with-async-mutex (async-scheduler-remote-mutex sched)
        (and (async-queue-empty? (async-scheduler-remote-queue sched))
             (or (not (async-group-parallel? group))
                 (with-async-mutex (async-scheduler-group-mutex group)
                   (and
                     (async-queue-empty?
                       (async-scheduler-group-ready-queue group))
                     (not (async-group-has-work? group))))))))))

(define async-sleep-until-next-timer
  (lambda (sched)
    (let ([timer (async-timer-heap-peek (async-scheduler-timers sched))])
      (if (not timer)
          ($oops 'run-async
            "async deadlock: no runnable tasks and no pending timers")
          (let* ([deadline (async-timer-deadline timer)]
                 [delta (max 0 (- deadline (async-monotonic-us)))])
            (sleep (make-time 'time-duration
                     (* (remainder delta 1000000) 1000)
                     (quotient delta 1000000))))))))

(define async-thread-idle-wait
  (if-feature pthreads
    (case-lambda
      [(sched) (async-thread-idle-wait sched (lambda () #f))]
      [(sched external-ready?)
       (let ([group ($async-scheduler-group sched)]
             [timer (async-timer-heap-peek (async-scheduler-timers sched))])
         (with-mutex (async-scheduler-remote-mutex sched)
           (when
             (and
               (not (external-ready?))
               (async-queue-empty? (async-scheduler-remote-queue sched))
               (or
                 (not (async-group-parallel? group))
                 (with-mutex (async-scheduler-group-mutex group)
                   (and
                     (async-queue-empty?
                       (async-scheduler-group-ready-queue group))
                     (not (async-group-has-work? group))
                     (not (async-scheduler-group-shutdown? group))))))
             (if (not timer)
                 (condition-wait (async-scheduler-remote-cond sched)
                                 (async-scheduler-remote-mutex sched))
                 (let* ([deadline (async-timer-deadline timer)]
                        [delta (max 0 (- deadline (async-monotonic-us)))]
                        [timeout (add-duration (current-time)
                                   (make-time 'time-duration
                                     (* (remainder delta 1000000) 1000)
                                     (quotient delta 1000000)))])
                   (condition-wait (async-scheduler-remote-cond sched)
                                   (async-scheduler-remote-mutex sched)
                                   timeout))))))])
    (case-lambda
      [(sched)
       ($oops 'run-async "thread idle wait requires thread support")]
      [(sched external-ready?)
       ($oops 'run-async "thread idle wait requires thread support")])))

(define async-idle-wait
  (lambda (sched)
    (dynamic-wind
      (lambda () (async-group-mark-idle! sched))
      (lambda ()
        (cond
          [(async-scheduler-virtual? sched)
           (let ([timer
                  (async-timer-heap-peek (async-scheduler-timers sched))])
             (if (not timer)
                 ($oops 'run-async
                   "async deadlock: no runnable tasks and no pending timers")
                 (async-scheduler-vtime-set! sched
                   (async-timer-deadline timer))))]
          [(async-scheduler-poll-proc sched)
           => (lambda (poll)
                ;; The preceding nonblocking poll may consume a remote wakeup
                ;; that arrived after the scheduler drained its queues.  A
                ;; submission after this locked recheck leaves uv_async
                ;; pending, so entering the blocking poll remains safe.
                (when (async-poll-ready-to-block? sched)
                  (poll sched #t)))]
          [(async-group-parallel? ($async-scheduler-group sched))
           (if-feature pthreads
             (async-thread-idle-wait sched)
             ($oops 'run-async
               "parallel scheduler groups require thread support"))]
          [else
           (if-feature pthreads
             (async-thread-idle-wait sched)
             (async-sleep-until-next-timer sched))]))
      (lambda () (async-group-unmark-idle! sched)))))

(define async-preemption-token (list 'async-preempted))
(define async-minimum-preemption-ticks 1000)

;;; Timer interrupts request a bounded, nonallocating transfer to the parked
;;; scheduler fiber.  The event epilogue performs the actual switch after the
;;; handler has unwound.
(define async-preemption-handler
  (lambda ()
    (let* ([sched ($async-scheduler)]
           [task
            (and (async-scheduler? sched)
                 (async-scheduler-current-task sched))]
           [scheduler-fiber
            (and task (async-scheduler-native-fiber sched))])
      (when (and task
                 scheduler-fiber
                 (eq? (async-task-state task) 'running)
                 (async-scheduler-preemption-ticks sched))
        ;; Timer delivery is a cancellation point for CPU-bound tasks.  Async
        ;; runtime critical sections defer Scheme events, so an exception is
        ;; raised only at a task-safe VM event boundary.
        (async-check-cancellation! task)
        ($call-with-native-fiber-preemption-window
          (lambda ()
            (unless ($native-fiber-preempt scheduler-fiber
                      async-preemption-token)
              (set-timer (async-scheduler-preemption-ticks sched)))))))))

;;; Install the handler once for a scheduler worker.  Individual dispatches
;;; only arm and disarm the timer, avoiding parameter and handler churn on
;;; every cooperative suspension.
(define call-with-async-preemption-handler
  (lambda (sched thunk)
    (if (not (async-scheduler-preemption-ticks sched))
        (thunk)
        (let* ([saved-handler (timer-interrupt-handler)]
               [saved-ticks (set-timer 0)])
          (dynamic-wind
            (lambda ()
              (timer-interrupt-handler async-preemption-handler)
              (set-timer 0))
            thunk
            (lambda ()
              (set-timer 0)
              (timer-interrupt-handler saved-handler)
              (set-timer saved-ticks)))))))

;;; Reserve the Chez tick timer only while a native task fiber is running.
(define call-with-async-preemption
  (lambda (sched thunk)
    (dynamic-wind
      (lambda () (set-timer (async-scheduler-preemption-ticks sched)))
      thunk
      (lambda () (set-timer 0)))))

(define async-switch-to-task
  (lambda (sched task)
    (let ([scheduler-fiber (async-scheduler-native-fiber sched)]
          [task-fiber (async-task-native-fiber task)]
          [payload (async-task-payload task)])
      (async-debug-check-native-fiber! 'async-switch-to-task
        scheduler-fiber '(running) #t)
      (async-debug-check-native-fiber! 'async-switch-to-task
        task-fiber '(new parked) #f)
      (async-task-payload-set! task #f)
      (let ([outcome
             ($native-fiber-claim-and-switch
               scheduler-fiber task-fiber payload)])
        (async-debug-check-native-fiber! 'async-switch-to-task
          scheduler-fiber '(running) #t)
        (async-debug-check-native-fiber! 'async-switch-to-task
          task-fiber '(claimed running parked finished) #f)
        outcome))))

(define async-run-task-once
  (lambda (sched task)
    (async-debug-check-owner! sched)
    (async-invariant (not (async-scheduler-current-task sched))
      "scheduler already has a running task" sched)
    (async-invariant (eq? (async-task-state task) 'ready)
      "scheduler selected a task that is not ready" task)
    (async-invariant (eq? (async-task-scheduler-group task)
                          ($async-scheduler-group sched))
      "scheduler selected a task from another group" task)
    (when async-debug-invariants?
      (with-async-mutex (async-scheduler-group-mutex
                          ($async-scheduler-group sched))
        (async-invariant
          (eq? (hashtable-ref
                 (async-scheduler-group-tasks ($async-scheduler-group sched))
                 (async-task-id task) #f)
               task)
          "scheduler selected a task missing from the registry" task)))
    (async-scheduler-exec-count-set! sched
      (fx+ 1 (async-scheduler-exec-count sched)))
    (let ([now (async-monotonic-us)]
          [worker-index (async-scheduler-group-index sched)])
      (with-async-mutex (async-task-mutex task)
        (let ([ready-start (async-task-ready-start-us task)]
              [wait-start (async-task-wait-start-us task)]
              [last-worker (async-task-last-worker-index task)])
          (when ready-start
            (async-task-queue-time-us-set! task
              (+ (async-task-queue-time-us task) (- now ready-start))))
          (when wait-start
            (async-task-wait-time-us-set! task
              (+ (async-task-wait-time-us task) (- now wait-start))))
          (when (and last-worker (not (fx= last-worker worker-index)))
            (async-task-migration-count-set! task
              (+ 1 (async-task-migration-count task))))
          (async-task-last-worker-index-set! task worker-index)
          (async-task-ready-start-us-set! task #f)
          (async-task-wait-start-us-set! task #f)
          (async-task-run-count-set! task (+ 1 (async-task-run-count task))))
        (async-task-current-wait-set! task #f)
        (async-task-wait-scheduler-set! task #f)
        (async-task-resume-pinned?-set! task #f)
        (async-task-suspension-state-set! task #f)
        (async-task-state-set! task 'running)
        (async-task-scheduler-set! task sched)))
    (async-scheduler-current-task-set! sched task)
    (async-trace-event! ($async-scheduler-group sched) sched task 'dispatch #f)
    (let* ([started (async-monotonic-us)]
           [outcome
            (if (async-scheduler-preemption-ticks sched)
                (call-with-async-preemption sched
                  (lambda () (async-switch-to-task sched task)))
                (async-switch-to-task sched task))]
           [stopped (async-monotonic-us)])
      (with-async-mutex (async-task-mutex task)
        (async-task-runtime-us-set! task
          (+ (async-task-runtime-us task) (- stopped started))))
      (async-scheduler-current-task-set! sched #f)
      (cond
        [(eq? outcome async-suspend-token)
         (async-trace-event! ($async-scheduler-group sched) sched task
           'suspend (async-task-current-wait task))
         (async-finish-suspension! task)
         task]
        [(eq? outcome async-yield-token)
         (async-task-state-set! task 'ready)
         (async-task-ready-start-us-set! task stopped)
         (async-trace-event! ($async-scheduler-group sched) sched task
           'yield #f)
         (async-atomic-box-add! (async-scheduler-wakeup-count-box sched) 1)
         (async-publish-ready! task sched)
         #f]
        [(eq? outcome async-preemption-token)
         (async-task-state-set! task 'ready)
         (async-task-ready-start-us-set! task stopped)
         (async-scheduler-preemption-count-set! sched
           (fx+ 1 (async-scheduler-preemption-count sched)))
         (async-trace-event! ($async-scheduler-group sched) sched task
           'preempt
           (async-trace-stack-sample ($async-scheduler-group sched) task))
         (async-publish-ready! task sched)
         #f]
        [(and (pair? outcome) (eq? (car outcome) 'done))
         (terminate-task! sched task 'completed outcome)
         #f]
        [(and (pair? outcome) (eq? (car outcome) 'failed))
         (if ($async-cancellation-condition? (cdr outcome))
             (terminate-task! sched task 'canceled outcome)
             (terminate-task! sched task 'failed outcome))
         #f]
        [else
         (terminate-task! sched task 'failed
           (cons 'raise
            (condition
              (make-error)
              (make-message-condition
                "invalid native-fiber scheduler outcome"))))
         #f]))))

(define async-scheduler-run
  (lambda (sched)
    (let ([current (async-scheduler-current-queue sched)]
          [next (async-scheduler-next-queue sched)])
      (let loop ()
        (async-debug-check-owner! sched)
        (async-debug-check-native-fiber! 'async-scheduler-run
          (async-scheduler-native-fiber sched) '(running) #t)
        (async-drain-remote! sched)
        (when (async-group-parallel? ($async-scheduler-group sched))
          (let ([task (async-take-work! sched)])
            (when task
              (if (and (eq? (async-task-state task) 'ready)
                       (async-task-group-runnable? task))
                  (begin
                    (async-debug-queue-claim! task 'next)
                    (async-queue-push! (async-scheduler-next-queue sched) task))
                  (async-remote-submit (async-task-scheduler task) task)))))
        (async-fire-due-timers! sched)
        (let ([poll (async-scheduler-poll-proc sched)])
          (when poll (poll sched #f)))
        (when (and (async-queue-empty? current)
                   (not (async-queue-empty? next)))
          ;; promote next-turn queue
          (async-fifo-head-set! current (async-fifo-head next))
          (async-fifo-tail-set! current (async-fifo-tail next))
          (async-fifo-head-set! next '())
          (async-fifo-tail-set! next '()))
        (if (async-queue-empty? current)
            (if (async-scheduler-group-shutdown?
                  ($async-scheduler-group sched))
                (void)
                (begin
                  (async-idle-wait sched)
                  (loop)))
            (begin
              ($async-scheduler-turn-count-set! sched (fx+ 1 ($async-scheduler-turn-count sched)))
              (let turn-loop ()
                (unless (async-queue-empty? current)
                  (let ([task (async-queue-pop! current)])
                    (async-debug-queue-release! task)
                    (when (eq? (async-task-state task) 'ready)
                      (let ([handoff (async-run-task-once sched task)])
                        (when handoff
                          (async-complete-suspension! handoff)))))
                  (turn-loop)))
              (loop)))))))

(define async-group-fail!
  (lambda (group condition)
    (let ([root
           (with-async-mutex (async-scheduler-group-mutex group)
             (unless (async-scheduler-group-failure group)
               (async-scheduler-group-failure-set! group condition))
             (async-scheduler-group-root-task group))])
      (when (and root (not (task-terminal? root)))
        (task-cancel! root condition))
      (async-group-shutdown! group))))

(define async-install-scheduler-fiber!
  (lambda (sched)
    (let ([fiber
           (or ($current-native-fiber)
               ($native-fiber-adopt
                 (if async-debug-invariants?
                     (constant native-fiber-flag-debug)
                     0)))])
      (unless (not (fx= (fxlogand ($native-fiber-flags fiber)
                                  (constant native-fiber-flag-scheduler))
                         0))
        ($oops 'run-async "cannot enter an async scheduler from a task fiber"))
      (async-scheduler-native-fiber-set! sched fiber))))

(define async-run-scheduler-thread
  (lambda (sched)
    (let ([old-sched ($async-scheduler)]
          [group ($async-scheduler-group sched)])
      (guard (c [else (async-group-fail! group c)])
        (dynamic-wind
          (lambda ()
            (async-install-scheduler-fiber! sched)
            ($async-scheduler sched)
            (async-scheduler-status-set! sched 'running)
            (async-scheduler-owner-thread-set! sched (get-thread-id)))
          (lambda ()
            (call-with-async-preemption-handler sched
              (lambda () (async-scheduler-run sched))))
          (lambda ()
            ($async-io-shutdown sched)
            (async-scheduler-status-set! sched 'shutdown)
            (async-scheduler-owner-thread-set! sched #f)
            ($async-scheduler old-sched)))))))


;;; -------------------------------------------------------------- observability
