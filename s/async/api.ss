;;; ------------------------------------------------------ public exports

(set! async-cancellation-condition? $async-cancellation-condition?)

(set! async-cancellation-reason $async-cancellation-reason)

(set! make-async-cancellation-condition
  (case-lambda
    [() ($make-async-cancellation-condition #f)]
    [(reason) ($make-async-cancellation-condition reason)]))

(set! channel-closed-condition? $channel-closed-condition?)

(set! channel-closed-reason $channel-closed-reason)

(set! make-channel-closed-condition
  (case-lambda
    [() ($make-channel-closed-condition #f)]
    [(reason) ($make-channel-closed-condition reason)]))

(set! async-context? $async-context?)

(set-who! make-async-context
  (case-lambda
    [() (async-make-context (async-current-context))]
    [(parent)
     (unless (or (not parent) (async-context? parent))
       ($oops who "~s is not an async context or #f" parent))
     (async-make-context parent)]))

(set-who! async-context-cancel!
  (case-lambda
    [(context) (async-context-cancel! context #f)]
    [(context reason)
     (unless (async-context? context)
       ($oops who "~s is not an async context" context))
     (async-context-cancel-core! context reason)
     (void)]))

(set-who! async-context-canceled?
  (lambda (context)
    (unless (async-context? context)
      ($oops who "~s is not an async context" context))
    (with-async-mutex (async-context-mutex context)
      (async-context-canceled?/raw context))))

(set-who! async-context-reason
  (lambda (context)
    (unless (async-context? context)
      ($oops who "~s is not an async context" context))
    (with-async-mutex (async-context-mutex context)
      (async-context-reason/raw context))))

(set-who! async-context-done-operation
  (lambda (context)
    (unless (async-context? context)
      ($oops who "~s is not an async context" context))
    (async-context-operation context)))

(set-who! async-context-with-timeout
  (lambda (parent seconds)
    (unless (async-context? parent)
      ($oops who "~s is not an async context" parent))
    (unless (async-valid-seconds? seconds)
      ($oops who "~s is not a nonnegative real number" seconds))
    (let ([sched ($async-scheduler)])
      (unless (and ($async-scheduler? sched)
                   (async-scheduler-current-task sched))
        ($oops who "timeout context creation outside of an async task"))
      (async-install-context-deadline! who (async-make-context parent)
        (+ (sched-now sched) (async-seconds->us seconds))))))

(set-who! async-context-with-deadline
  (lambda (parent deadline)
    (unless (async-context? parent)
      ($oops who "~s is not an async context" parent))
    (unless (and (time? deadline)
                 (eq? (time-type deadline) 'time-monotonic))
      ($oops who "~s is not a monotonic time" deadline))
    (let ([sched ($async-scheduler)])
      (unless (and ($async-scheduler? sched)
                   (async-scheduler-current-task sched))
        ($oops who "deadline context creation outside of an async task"))
      (when (async-scheduler-virtual? sched)
        ($oops who "absolute deadlines require a real scheduler clock"))
      (async-install-context-deadline! who (async-make-context parent)
        (+ (* (time-second deadline) 1000000)
           (quotient (time-nanosecond deadline) 1000))))))

(set-who! current-async-context
  (lambda () (async-current-context)))

(set-who! call-with-async-context
  (lambda (context thunk)
    (unless (async-context? context)
      ($oops who "~s is not an async context" context))
    (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
    (let ([task (async-current-task/required who)]
          [outside-context #f])
      (async-dynamic-wind
        (lambda ()
          (set! outside-context (async-task-context-override task))
          (async-task-context-override-set! task context))
        thunk
        (lambda ()
          (async-task-context-override-set! task outside-context))))))

(set-who! call-with-async-timeout
  (lambda (seconds thunk)
    (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
    (let ([parent (async-current-context)])
      (unless parent
        ($oops who "call outside of an async task"))
      (let ([context (async-context-with-timeout parent seconds)])
        (async-dynamic-wind
          (lambda () (void))
          (lambda () (call-with-async-context context thunk))
          (lambda ()
            (async-context-cancel-core! context 'scope-exited)))))))

(set-who! perform-operation/context
  (lambda (context op)
    (unless (async-context? context)
      ($oops who "~s is not an async context" context))
    (unless (operation? op) ($oops who "~s is not an operation" op))
    (call-with-async-context context
      (lambda () (perform-operation op)))))

(record-writer (type-descriptor async-task)
  (lambda (r p wr)
    (fprintf p "#<task ~a~a ~a>"
      (task-id r)
      (if (task-name r) (format " ~s" (task-name r)) "")
      (task-state r))))

(record-writer (type-descriptor async-context)
  (lambda (r p wr)
    (fprintf p "#<async-context ~a>"
      (if (async-context-canceled?/raw r) 'canceled 'active))))

(record-writer (type-descriptor async-scheduler)
  (lambda (r p wr)
    (fprintf p "#<async-scheduler ~a tasks=~a turns=~a>"
      (async-scheduler-status r)
      (async-scheduler-group-task-count ($async-scheduler-group r))
      ($async-scheduler-turn-count r))))

(record-writer (type-descriptor async-channel)
  (lambda (r p wr)
    (fprintf p "#<channel capacity=~a~a>"
      (async-channel-capacity r)
      (if (async-channel-closed? r) " closed" ""))))

(record-writer (type-descriptor async-future)
  (lambda (r p wr)
    (fprintf p "#<future ~a>"
      (if (eq? (unbox (async-future-state r)) 'waiting) 'pending 'fulfilled))))

(set! async-scheduler? $async-scheduler?)

(set! task? $async-task?)

(set! task-group? $async-task-group?)

(set! operation? $async-operation?)

(set! future? $async-future?)

(set! channel? $async-channel?)

(set! async-mutex? $async-mutex?)

(set! async-rw-mutex? $async-rw-mutex?)

(set! async-wait-group? $async-wait-group?)

(set! async-once? $async-once?)

(set! async-condition? $async-condition?)

(set! task-id
  (lambda (task)
    (unless (task? task) ($oops 'task-id "~s is not a task" task))
    (async-task-id task)))

(set! task-name
  (lambda (task)
    (unless (task? task) ($oops 'task-name "~s is not a task" task))
    (async-task-name task)))

(set! task-state
  (lambda (task)
    (unless (task? task) ($oops 'task-state "~s is not a task" task))
    (async-task-state task)))

(set! task-scheduler
  (lambda (task)
    (unless (task? task) ($oops 'task-scheduler "~s is not a task" task))
    (async-task-scheduler task)))

(set! task-context
  (lambda (task)
    (unless (task? task) ($oops 'task-context "~s is not a task" task))
    (async-task-context task)))

(set-who! make-task-group
  (lambda ()
    (make-async-group #f (async-make-context (async-current-context)))))

(set-who! task-group-wait
  (lambda (grp)
    (unless (task-group? grp) ($oops who "~s is not a task group" grp))
    (async-check-operation-entry! (async-current-task/required who))
    (unless (with-async-mutex (async-task-group-mutex grp)
              (async-group-empty?/locked grp))
      (perform-operation (group-empty-operation grp)))
    (let loop ()
      (let ([u
             (with-async-mutex (async-task-group-mutex grp)
               (and (pair? (async-task-group-unobserved grp))
                    (let ([u (car (async-task-group-unobserved grp))])
                      (async-task-group-unobserved-set! grp
                        (cdr (async-task-group-unobserved grp)))
                      u)))])
        (when u
          (if (async-task-observe-failure! (car u))
              (raise (cdr u))
              (loop)))))
    (void)))

(set-who! make-async-mutex
  (lambda ()
    (make-async-fiber-mutex% (make-async-os-mutex) #f
      (make-async-wait-queue))))

(set-who! async-mutex-acquire-operation
  (lambda (mutex)
    (unless (async-mutex? mutex)
      ($oops who "~s is not an async mutex" mutex))
    (async-fiber-mutex-acquire-operation mutex)))

(set-who! async-mutex-acquire
  (lambda (mutex)
    (unless (async-mutex? mutex)
      ($oops who "~s is not an async mutex" mutex))
    (let ([task (async-mutex-current-task who)])
      (async-check-operation-entry! task)
      (unless (async-fiber-mutex-try-acquire! mutex task who)
        (perform-operation (async-fiber-mutex-acquire-operation mutex))))))

(set-who! async-mutex-release!
  (lambda (mutex)
    (unless (async-mutex? mutex)
      ($oops who "~s is not an async mutex" mutex))
    (let ([task (async-mutex-current-task who)])
      (let ([action
             (with-async-mutex (async-fiber-mutex-mutex mutex)
               (unless (eq? (async-fiber-mutex-owner mutex) task)
                 ($oops who "current task does not own the mutex"))
               (async-fiber-mutex-owner-set! mutex #f)
               (async-mutex-handoff! mutex))])
        (async-task-remove-owned-mutex! task mutex)
        (when action (action))))
    (void)))

(set-who! call-with-async-mutex
  (lambda (mutex thunk)
    (unless (async-mutex? mutex)
      ($oops who "~s is not an async mutex" mutex))
    (unless (procedure? thunk)
      ($oops who "~s is not a procedure" thunk))
    (async-mutex-acquire mutex)
    (let ([released? #f])
      (define release!
        (lambda ()
          (unless released?
            (set! released? #t)
            (async-mutex-release! mutex))))
      (guard (condition
               [else
                (release!)
                (raise condition)])
        (call-with-values thunk
          (lambda result*
            (release!)
            (apply values result*)))))))

(set-who! make-async-rw-mutex
  (lambda ()
    (make-async-rw-mutex% (make-async-os-mutex)
      #f (make-eq-hashtable) (make-async-wait-queue))))

(set-who! async-rw-mutex-acquire-operation
  (lambda (mutex)
    (unless (async-rw-mutex? mutex)
      ($oops who "~s is not an async rw mutex" mutex))
    (async-rw-mutex-acquire-operation/raw mutex 'write)))

(set-who! async-rw-mutex-read-acquire-operation
  (lambda (mutex)
    (unless (async-rw-mutex? mutex)
      ($oops who "~s is not an async rw mutex" mutex))
    (async-rw-mutex-acquire-operation/raw mutex 'read)))

(set-who! async-rw-mutex-acquire
  (lambda (mutex)
    (unless (async-rw-mutex? mutex)
      ($oops who "~s is not an async rw mutex" mutex))
    (let ([task (async-mutex-current-task who)])
      (async-check-operation-entry! task)
      (unless (async-rw-mutex-try-acquire! mutex 'write task)
        (perform-operation
          (async-rw-mutex-acquire-operation/raw mutex 'write))))))

(set-who! async-rw-mutex-read-acquire
  (lambda (mutex)
    (unless (async-rw-mutex? mutex)
      ($oops who "~s is not an async rw mutex" mutex))
    (let ([task (async-mutex-current-task who)])
      (async-check-operation-entry! task)
      (unless (async-rw-mutex-try-acquire! mutex 'read task)
        (perform-operation
          (async-rw-mutex-acquire-operation/raw mutex 'read))))))

(set-who! async-rw-mutex-release!
  (lambda (mutex)
    (unless (async-rw-mutex? mutex)
      ($oops who "~s is not an async rw mutex" mutex))
    (let ([task (async-mutex-current-task who)])
      (let ([action
             (with-async-mutex (async-rw-mutex-mutex mutex)
               (unless (eq? (async-rw-mutex-writer mutex) task)
                 ($oops who "current task does not own the write lock"))
               (async-rw-mutex-writer-set! mutex #f)
               (async-rw-mutex-handoff! mutex))])
        (async-task-remove-owned-mutex! task mutex)
        (when action (action))))
    (void)))

(set-who! async-rw-mutex-read-release!
  (lambda (mutex)
    (unless (async-rw-mutex? mutex)
      ($oops who "~s is not an async rw mutex" mutex))
    (let ([task (async-mutex-current-task who)])
      (let ([action
             (with-async-mutex (async-rw-mutex-mutex mutex)
               (unless (async-rw-mutex-remove-reader! mutex task)
                 ($oops who "current task does not own a read lock"))
               (and (async-rw-mutex-idle? mutex)
                    (async-rw-mutex-handoff! mutex)))])
        (when action (action))))
    (void)))

(set-who! call-with-async-rw-mutex
  (lambda (mutex thunk) (async-call-with-rw-lock mutex thunk #f)))

(set-who! call-with-async-read-mutex
  (lambda (mutex thunk) (async-call-with-rw-lock mutex thunk #t)))

(set-who! make-async-wait-group
  (case-lambda
    [() (make-async-wait-group 0)]
    [(count)
     (unless (and (integer? count) (exact? count) (>= count 0))
       ($oops who "~s is not a nonnegative exact integer" count))
     (make-async-wait-group% (make-async-os-mutex) count
       (make-async-wait-queue))]))

(set-who! async-wait-group-add!
  (lambda (group delta)
    (unless (async-wait-group? group)
      ($oops who "~s is not an async wait group" group))
    (unless (and (integer? delta) (exact? delta))
      ($oops who "~s is not an exact integer" delta))
    (let ([waiters
           (with-async-mutex (async-wait-group-mutex group)
             (let ([count (+ (async-wait-group-count group) delta)])
               (when (< count 0)
                 ($oops who "wait group counter would become negative"))
               (async-wait-group-count-set! group count)
               (if (= count 0)
                   (async-wait-queue-drain!
                     (async-wait-group-waiters group))
                   '())))])
      (for-each
        (lambda (waiter) ((cdr waiter) (cons 'values '())))
        waiters))
    (void)))

(set-who! async-wait-group-done!
  (lambda (group) (async-wait-group-add! group -1)))

(set-who! async-wait-group-wait-operation
  (lambda (group)
    (unless (async-wait-group? group)
      ($oops who "~s is not an async wait group" group))
    (async-wait-group-wait-operation/raw group)))

(set-who! async-wait-group-wait
  (lambda (group)
    (unless (async-wait-group? group)
      ($oops who "~s is not an async wait group" group))
    (async-check-operation-entry! (async-current-task/required who))
    (unless (with-async-mutex (async-wait-group-mutex group)
              (= (async-wait-group-count group) 0))
      (perform-operation (async-wait-group-wait-operation/raw group)))))

(set-who! spawn-task/async-wait-group
  (lambda (group thunk . options)
    (unless (async-wait-group? group)
      ($oops who "~s is not an async wait group" group))
    (unless (procedure? thunk)
      ($oops who "~s is not a procedure" thunk))
    (let* ([parent (async-mutex-current-task who)]
           [old-shield? (async-task-cancel-shield? parent)]
           [added? #f]
           [spawned? #f]
           [completion-claimed (box #f)])
      (define finish!
        (lambda ()
          (when (box-cas! completion-claimed #f #t)
            (async-wait-group-done! group))))
      ;; Add and spawn form one cancellation-safe publication step. Once the
      ;; child exists, its termination action is the final backstop, including
      ;; cancellation before the entry thunk starts.
      (guard (condition
               [else
                (async-task-cancel-shield?-set! parent old-shield?)
                (when (and added? (not spawned?))
                  (finish!))
                (raise condition)])
        (async-task-cancel-shield?-set! parent #t)
        (async-wait-group-add! group 1)
        (set! added? #t)
        (let ([task (async-spawn-task who thunk options (list finish!))])
          (set! spawned? #t)
          (async-task-cancel-shield?-set! parent old-shield?)
          task)))))

(set-who! make-async-once
  (lambda ()
    (make-async-once% (make-async-os-mutex) 'pending #f
      (make-async-wait-queue))))

(set-who! async-once-run!
  (lambda (once thunk)
    (unless (async-once? once)
      ($oops who "~s is not an async once" once))
    (unless (procedure? thunk)
      ($oops who "~s is not a procedure" thunk))
    (let* ([task (async-mutex-current-task who)]
           [old-shield? (async-task-cancel-shield? task)])
      ;; Do not let timed preemption inject cancellation after this caller is
      ;; published as the initializer but before its cleanup guard exists.
      (async-task-cancel-shield?-set! task #t)
      (guard (condition
               [else
                (async-task-cancel-shield?-set! task old-shield?)
                (raise condition)])
        (let ([action
               (with-async-mutex (async-once-mutex once)
                 (case (async-once-state once)
                   [(pending)
                    (async-once-state-set! once 'running)
                    (async-once-owner-set! once task)
                    'run]
                   [(done) 'done]
                   [else
                    (if (eq? (async-once-owner once) task)
                        ($oops who "recursive async once invocation")
                        'wait)]))])
          (case action
            [(done)
             (async-task-cancel-shield?-set! task old-shield?)
             (void)]
            [(wait)
             (async-task-cancel-shield?-set! task old-shield?)
             (perform-operation (async-once-wait-operation once))
             (void)]
            [else
             (letrec ([finish!
                       (lambda ()
                         (let ([waiters
                                (with-async-mutex (async-once-mutex once)
                                  (async-once-state-set! once 'done)
                                  (async-once-owner-set! once #f)
                                  (async-wait-queue-drain!
                                    (async-once-waiters once)))])
                           (for-each
                             (lambda (waiter)
                               ((cdr waiter) (cons 'values '())))
                             waiters)))])
               (guard (condition
                        [else
                         (async-task-cancel-shield?-set! task #t)
                         (finish!)
                         (async-task-cancel-shield?-set! task old-shield?)
                         (raise condition)])
                 (async-task-cancel-shield?-set! task old-shield?)
                 (thunk)
                 (async-task-cancel-shield?-set! task #t)
                 (finish!)
                 (async-task-cancel-shield?-set! task old-shield?)
                 (void)))]))))))

(set-who! make-async-condition
  (lambda (lock)
    (unless (or (async-mutex? lock) (async-rw-mutex? lock))
      ($oops who "~s is not an async mutex or async rw mutex" lock))
    (make-async-condition% (make-async-os-mutex) lock
      (make-async-wait-queue))))

(set! async-condition-mutex
  (lambda (condition)
    (unless (async-condition? condition)
      ($oops 'async-condition-mutex "~s is not an async condition" condition))
    (async-condition-lock condition)))

(set-who! async-condition-wait
  (lambda (condition)
    (unless (async-condition? condition)
      ($oops who "~s is not an async condition" condition))
    (let* ([task (async-mutex-current-task who)]
           [lock (async-condition-lock condition)]
           [released-box (box #f)]
           [old-shield? (async-task-cancel-shield? task)])
      (unless (async-lock-owned-by? lock task)
        ($oops who "current task does not own the condition mutex"))
      (letrec ([reacquire!
                (lambda ()
                  ;; Delivery or nack publishes this shield before the task is
                  ;; runnable. Keep it installed until ownership is restored.
                  (async-task-cancel-shield?-set! task #t)
                  (async-lock-acquire! lock)
                  (set-box! released-box #f))])
        (guard (condition
                 [else
                  (when (unbox released-box) (reacquire!))
                  (async-task-cancel-shield?-set! task old-shield?)
                  (raise condition)])
          (perform-operation
            (async-condition-wait-operation
              condition lock task released-box))
          (reacquire!)
          (async-task-cancel-shield?-set! task old-shield?)
          (void))))))

(set-who! async-condition-signal!
  (lambda (condition)
    (unless (async-condition? condition)
      ($oops who "~s is not an async condition" condition))
    (let ([publish
           (with-async-mutex (async-condition-guard condition)
             (let loop ()
               (let ([node (async-wait-queue-pop!
                             (async-condition-waiters condition))])
                 (and node
                      (let* ([waiter (async-wait-node-value node)]
                             [reservation
                              ((cdr waiter) 'reserve (cons 'values '()))])
                        (if reservation
                            (async-delivery-prepare! reservation)
                            (loop)))))))])
      (when publish (publish)))
    (void)))

(set-who! async-condition-broadcast!
  (lambda (condition)
    (unless (async-condition? condition)
      ($oops who "~s is not an async condition" condition))
    (let ([waiters
           (with-async-mutex (async-condition-guard condition)
             (async-wait-queue-drain!
               (async-condition-waiters condition)))])
      (for-each
        (lambda (waiter) ((cdr waiter) (cons 'values '())))
        waiters))
    (void)))

(set-who! make-operation
  (case-lambda
    [(try block)
     (make-operation try block (lambda (vals) vals) (lambda (ss) (void)))]
    [(try block wrap)
     (make-operation try block wrap (lambda (ss) (void)))]
    [(try block wrap nack)
     (unless (procedure? try) ($oops who "~s is not a procedure" try))
     (unless (procedure? block) ($oops who "~s is not a procedure" block))
     (unless (procedure? wrap) ($oops who "~s is not a procedure" wrap))
     (unless (procedure? nack) ($oops who "~s is not a procedure" nack))
     (make-async-operation try block wrap nack)]))

(set-who! always-operation
  (lambda vals
    (make-async-operation
      (lambda (ss) (cons 'values vals))
      (lambda (ss deliver) ($oops 'always-operation "cannot block"))
      (lambda (vals) vals)
      (lambda (ss) (void)))))

(set-who! never-operation
  (lambda ()
    (make-async-operation
      (lambda (ss) #f)
      (lambda (ss deliver) '(never))
      (lambda (vals) vals)
      (lambda (ss) (void)))))

(set-who! wrap-operation
  (lambda (op f)
    (unless (operation? op) ($oops who "~s is not an operation" op))
    (unless (procedure? f) ($oops who "~s is not a procedure" f))
    (make-async-operation
      (operation-try op)
      (operation-block op)
      (lambda (vals)
        (call-with-values
          (lambda () (apply f ((operation-wrap op) vals)))
          list))
      (operation-nack op))))

(set-who! choice-operation
  (lambda ops
    (for-each
      (lambda (op)
        (unless (operation? op) ($oops who "~s is not an operation" op)))
      ops)
    (let ([ops (list->vector ops)] [start (box 0)])
      (define next-start!
        (lambda (n)
          (let loop ()
            (let ([s (unbox start)])
              (if (box-cas! start s (fxmod (fx+ s 1) n))
                  s
                  (loop))))))
      (make-async-operation
        (lambda (ss)
          (let ([n (vector-length ops)])
            (if (fx= n 0)
                #f
                (let ([s (next-start! n)])
                  (let loop ([i 0])
                    (if (fx= i n)
                        #f
                        (let* ([op (vector-ref ops (fxmod (fx+ s i) n))]
                               [r ((operation-try op) ss)])
                          (if r
                              (if (eq? (car r) 'values)
                                  (cons 'values ((operation-wrap op) (cdr r)))
                                  r)
                              (loop (fx+ i 1))))))))))
        (lambda (ss deliver)
          (let ([n (vector-length ops)])
            (if (fx= n 0)
                '(choice)
                (let ([descs
                       (let f ([i 0])
                         (if (fx= i n)
                             '()
                             (let* ([op (vector-ref ops i)]
                                    [desc
                                     (and (async-sync-state-live? ss)
                                          ((operation-block op) ss
                                            (let ([transform
                                                   (lambda (payload)
                                                     (if (eq? (car payload) 'values)
                                                         (cons 'values
                                                           (list
                                                             (make-async-choice-result
                                                               op (cdr payload))))
                                                         payload))]
                                                  [nack-losers
                                                   (lambda ()
                                                     (do ([j 0 (fx+ j 1)])
                                                         ((fx= j n))
                                                       (unless (fx= i j)
                                                         ((operation-nack
                                                            (vector-ref ops j))
                                                          ss))))])
                                              (case-lambda
                                                [(payload)
                                                 (let ([won?
                                                        (deliver
                                                          (transform payload))])
                                                   (when won? (nack-losers))
                                                   won?)]
                                                [(command payload)
                                                 (let ([reservation
                                                        (deliver command
                                                          (transform payload))])
                                                   (when reservation
                                                     (async-delivery-add-action!
                                                       reservation nack-losers))
                                                   reservation)]))))])
                               (cons desc (f (fx+ i 1))))))])
                  (list 'choice (filter (lambda (d) d) descs))))))
        (lambda (vals)
          (if (and (pair? vals)
                   (null? (cdr vals))
                   (async-choice-result? (car vals)))
              (let ([r (car vals)])
                ((operation-wrap (async-choice-result-operation r))
                 (async-choice-result-values r)))
              vals))
        (lambda (ss)
          (vector-for-each
            (lambda (op) ((operation-nack op) ss))
            ops))))))

(set-who! perform-operation
  (lambda (op)
    (unless (operation? op) ($oops who "~s is not an operation" op))
    (let ([sched ($async-scheduler)])
      (unless (async-scheduler? sched)
        ($oops who "perform-operation outside of an async scheduler"))
      (let ([task (async-scheduler-current-task sched)])
        (unless task
          ($oops who "perform-operation outside of an async task"))
        (let* ([context (async-check-operation-entry! task)]
               [ss (make-async-sync-state)])
          (let ([r ((operation-try op) ss)])
            (if r
                (async-deliver-operation-result op r)
                (async-deliver-operation-result op
                  ($async-suspend sched task ss
                    (lambda (ss*)
                      (let* ([op-deliver #f]
                             [context-deliver #f]
                             [nack
                              (lambda ()
                                ((operation-nack op) ss)
                                (when context
                                  (async-context-unregister-waiter!
                                    context ss)))])
                        (async-task-nack-thunk-set! task nack)
                        (async-sync-begin-registration! ss nack)
                        (let* ([deliver ($async-make-deliver ss task)]
                               [op-deliver
                                (case-lambda
                                  [(payload)
                                   (let ([won? (deliver payload)])
                                     (when (and won? context)
                                       (async-context-unregister-waiter!
                                         context ss))
                                     won?)]
                                  [(command payload)
                                   (let ([reservation
                                          (deliver command payload)])
                                     (when (and reservation context)
                                       (async-delivery-add-action! reservation
                                         (lambda ()
                                           (async-context-unregister-waiter!
                                             context ss))))
                                     reservation)])]
                               [desc ((operation-block op) ss op-deliver)]
                               [context-registration
                                (and context
                                     (begin
                                       (set! context-deliver
                                         (lambda (payload)
                                           (let ([won?
                                                  (deliver
                                                    (cons 'raise
                                                      (async-context-cancellation-condition
                                                        context)))])
                                             (when won?
                                               ((operation-nack op) ss))
                                             won?)))
                                       (async-context-register-waiter!
                                         context ss context-deliver)))])
                          (when (eq? context-registration 'canceled)
                            (context-deliver #f))
                          (when (async-sync-end-registration! ss)
                            ($async-cancel-waiting-task task))
                          (if context
                              (list 'choice
                                (list desc (list 'context context)))
                              desc)))))))))))))

(set-who! sleep-operation
  (lambda (seconds)
    (unless (async-valid-seconds? seconds)
      ($oops who "~s is not a nonnegative real number" seconds))
    (let ([token (list 'sleep-operation)])
      (make-async-operation
        (lambda (ss)
          (and (<= seconds 0) (cons 'values '())))
        (lambda (ss deliver)
          (let* ([sched ($async-scheduler)]
                 [deadline (+ (sched-now sched) (async-seconds->us seconds))]
                 [timer (async-schedule-timer! sched deadline deliver)])
            (async-sync-slot-set! ss token timer)
            (list 'sleep deadline)))
        (lambda (vals) vals)
        (lambda (ss)
          (let ([timer (async-sync-slot-ref ss token #f)])
            (when timer
              (async-sync-slot-delete! ss token)
              (async-cancel-timer! timer))))))))

(set-who! async-sleep
  (lambda (seconds)
    (unless (async-valid-seconds? seconds)
      ($oops who "~s is not a nonnegative real number" seconds))
    (perform-operation (sleep-operation seconds))
    (void)))

(set-who! make-future
  (lambda ()
    (make-async-future% (box 'waiting) (make-async-wait-queue)
      (make-async-os-mutex))))

(set-who! future-fulfil!
  (lambda (f . vals)
    (unless (future? f) ($oops who "~s is not a future" f))
    (future-complete! f (cons 'values vals))))

(set-who! future-fail!
  (lambda (f condition)
    (unless (future? f) ($oops who "~s is not a future" f))
    (future-complete! f (cons 'raise condition))))

(set-who! future-operation
  (lambda (f)
    (unless (future? f) ($oops who "~s is not a future" f))
    (let ([token (list 'future-operation)])
      (make-async-operation
      (lambda (ss)
        (future-ready-payload f))
      (lambda (ss deliver)
        (with-async-mutex (async-future-mutex f)
          (let ([state (unbox (async-future-state f))])
            (if (and (pair? state) (eq? (car state) 'done))
                (begin (deliver (cdr state)) #f)
                (begin
                  (async-sync-slot-set! ss token
                    (async-wait-queue-add! (async-future-waiters f)
                      (cons ss deliver)))
                  (list 'future))))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-future-mutex f)
          (let ([node (async-sync-slot-ref ss token #f)])
            (when node
              (async-wait-queue-remove! (async-future-waiters f) node)
              (async-sync-slot-delete! ss token)))))))))

(set-who! future-get
  (lambda (f)
    (unless (future? f) ($oops who "~s is not a future" f))
    (let ([task (async-current-task/required who)])
      (async-check-operation-entry! task)
      (let ([payload (future-ready-payload f)])
        (if payload
            (async-return-payload payload)
            (perform-operation (future-operation f)))))))

(set-who! make-channel
  (case-lambda
    [() (make-async-channel% 0 (make-async-os-mutex) #f 0 0
          (make-async-wait-queue) (make-async-wait-queue) #f #f)]
    [(capacity)
     (unless (and (fixnum? capacity) (fx>= capacity 0))
       ($oops who "~s is not a nonnegative fixnum" capacity))
     (make-async-channel% capacity (make-async-os-mutex)
       (and (fx> capacity 0) (make-vector capacity)) 0 0
       (make-async-wait-queue) (make-async-wait-queue) #f #f)]))

(set-who! channel-close!
  (case-lambda
    [(ch) (channel-close! ch #f)]
    [(ch reason)
     (unless (channel? ch) ($oops who "~s is not a channel" ch))
     (let-values ([(puts gets)
                   (with-async-mutex (async-channel-mutex ch)
                     (if (async-channel-closed? ch)
                         (values '() '())
                         (begin
                           (async-channel-closed?-set! ch #t)
                           (async-channel-close-reason-set! ch reason)
                           (async-invariant
                             (or (fx= (async-channel-bcount ch) 0)
                                 (async-wait-queue-empty?
                                   (async-channel-gets ch)))
                             "buffered channel has a live receiver at close"
                             ch)
                           (let ([puts
                                  (async-wait-queue-drain!
                                    (async-channel-puts ch))]
                                 [all-gets
                                  (async-wait-queue-drain!
                                    (async-channel-gets ch))])
                             (values puts
                               (if (fx= (async-channel-bcount ch) 0)
                                   all-gets
                                   '()))))))])
       (let ([payload (async-channel-put-closed-payload ch)])
         (for-each (lambda (p) ((caddr p) payload)) puts))
       (let ([payload (async-channel-receive-closed-payload)])
         (for-each (lambda (g) ((cdr g) payload)) gets)))
     (void)]))

(set-who! channel-closed?
  (lambda (ch)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (with-async-mutex (async-channel-mutex ch)
      (async-channel-closed? ch))))

(set! async-channel-try-put!
  (lambda (ch v)
    (let-values ([(payload publications)
                  (with-async-mutex (async-channel-mutex ch)
                    (cond
                      [(async-channel-closed? ch)
                       (values (async-channel-put-closed-payload ch) '())]
                      [(async-channel-reserve-getter! ch v)
                       => (lambda (reservation)
                            (values (cons 'values '())
                              (async-delivery-prepare-all!
                                (list (cdr reservation)))))]
                      [(and (fx> (async-channel-capacity ch) 0)
                            (fx< (async-channel-bcount ch)
                              (async-channel-capacity ch)))
                       (async-buffer-push! ch v)
                       (values (cons 'values '()) '())]
                      [else (values #f '())]))])
      (async-delivery-publish-all! publications)
      payload)))

(set! async-channel-try-receive!
  (lambda (ch)
    (let-values ([(payload publications)
                  (with-async-mutex (async-channel-mutex ch)
                    (cond
                      [(and (fx> (async-channel-capacity ch) 0)
                            (fx> (async-channel-bcount ch) 0))
                       (let* ([v (async-buffer-pop! ch)]
                              [putter (async-channel-reserve-putter! ch)])
                         (when putter
                           (async-buffer-push! ch (vector-ref putter 0)))
                         (values (cons 'values (list v #t))
                           (if putter
                               (async-delivery-prepare-all!
                                 (list (vector-ref putter 2)))
                               '())))]
                      [(async-channel-reserve-putter! ch)
                       => (lambda (putter)
                            (values
                              (cons 'values (list (vector-ref putter 0) #t))
                              (async-delivery-prepare-all!
                                (list (vector-ref putter 2)))))]
                      [(async-channel-closed? ch)
                       (values (async-channel-receive-closed-payload) '())]
                      [else (values #f '())]))])
      (async-delivery-publish-all! publications)
      payload)))

(set-who! channel-put-operation
  (lambda (ch v)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (let ([token (list 'channel-put-operation)])
      (make-async-operation
        (lambda (ss)
          (async-channel-try-put! ch v))
        (lambda (ss deliver)
          (let-values ([(descriptor publications)
                        (with-async-mutex (async-channel-mutex ch)
                          (cond
                            [(async-channel-closed? ch)
                             (let ([reservation
                                    (deliver 'reserve
                                      (async-channel-put-closed-payload ch))])
                               (values #f
                                 (if reservation
                                     (async-delivery-prepare-all!
                                       (list reservation))
                                     '())))]
                            [(async-channel-reserve-getter! ch v)
                             => (lambda (getter)
                                  (let ([putter
                                         (deliver 'reserve
                                           (cons 'values '()))])
                                    (if putter
                                        (values #f
                                          (async-delivery-prepare-all!
                                            (list (cdr getter) putter)))
                                        (begin
                                          (async-delivery-rollback! (cdr getter))
                                          (async-wait-queue-reinsert-front!
                                            (async-channel-gets ch) (car getter))
                                          (values #f '())))))]
                            [(and (fx> (async-channel-capacity ch) 0)
                                  (fx< (async-channel-bcount ch)
                                    (async-channel-capacity ch)))
                             (let ([putter
                                    (deliver 'reserve (cons 'values '()))])
                               (when putter (async-buffer-push! ch v))
                               (values #f
                                 (if putter
                                     (async-delivery-prepare-all!
                                       (list putter))
                                     '())))]
                            [else
                             (async-sync-slot-set! ss token
                               (async-wait-queue-add! (async-channel-puts ch)
                                 (list v ss deliver)))
                             (values (list 'channel-put ch) '())]))])
            (async-delivery-publish-all! publications)
            descriptor))
        (lambda (vals) vals)
        (lambda (ss)
          (with-async-mutex (async-channel-mutex ch)
            (let ([node (async-sync-slot-ref ss token #f)])
              (when node
                (async-wait-queue-remove! (async-channel-puts ch) node)
                (async-sync-slot-delete! ss token)))))))))

(set-who! channel-receive-operation
  (lambda (ch)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (let ([token (list 'channel-receive-operation)])
      (make-async-operation
        (lambda (ss)
          (async-channel-try-receive! ch))
        (lambda (ss deliver)
          (let-values ([(descriptor publications)
                        (with-async-mutex (async-channel-mutex ch)
                          (cond
                            [(and (fx> (async-channel-capacity ch) 0)
                                  (fx> (async-channel-bcount ch) 0))
                             (let ([receiver
                                    (deliver 'reserve
                                      (cons 'values
                                        (list
                                          (vector-ref
                                            (async-channel-buffer ch)
                                            (async-channel-bstart ch))
                                          #t)))])
                               (if receiver
                                   (let* ([v (async-buffer-pop! ch)]
                                          [putter
                                           (async-channel-reserve-putter! ch)])
                                     (when putter
                                       (async-buffer-push! ch
                                         (vector-ref putter 0)))
                                     (values #f
                                       (async-delivery-prepare-all!
                                         (append
                                           (if putter
                                               (list (vector-ref putter 2)) '())
                                           (list receiver)))))
                                   (values #f '())))]
                            [(async-channel-reserve-putter! ch)
                             => (lambda (putter)
                                  (let ([receiver
                                         (deliver 'reserve
                                           (cons 'values
                                             (list (vector-ref putter 0) #t)))])
                                    (if receiver
                                        (values #f
                                          (async-delivery-prepare-all!
                                            (list (vector-ref putter 2) receiver)))
                                        (begin
                                          (async-delivery-rollback!
                                            (vector-ref putter 2))
                                          (async-wait-queue-reinsert-front!
                                            (async-channel-puts ch)
                                            (vector-ref putter 1))
                                          (values #f '())))))]
                            [(async-channel-closed? ch)
                             (let ([receiver
                                    (deliver 'reserve
                                      (async-channel-receive-closed-payload))])
                               (values #f
                                 (if receiver
                                     (async-delivery-prepare-all!
                                       (list receiver))
                                     '())))]
                            [else
                             (async-sync-slot-set! ss token
                               (async-wait-queue-add! (async-channel-gets ch)
                                 (cons ss deliver)))
                             (values (list 'channel-receive ch) '())]))])
            (async-delivery-publish-all! publications)
            descriptor))
        (lambda (vals) vals)
        (lambda (ss)
          (with-async-mutex (async-channel-mutex ch)
            (let ([node (async-sync-slot-ref ss token #f)])
              (when node
                (async-wait-queue-remove! (async-channel-gets ch) node)
                (async-sync-slot-delete! ss token)))))))))

(set-who! channel-get-operation
  (lambda (ch)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (wrap-operation (channel-receive-operation ch)
      (lambda (v open?)
        (if open?
            v
            (raise (async-channel-closed-condition ch)))))))

(set-who! channel-put
  (lambda (ch v)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (async-check-operation-entry! (async-current-task/required who))
    (let ([payload (async-channel-try-put! ch v)])
      (if payload
          (async-return-payload payload)
          (perform-operation (channel-put-operation ch v))))
    (void)))

(set-who! channel-get
  (lambda (ch)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (async-check-operation-entry! (async-current-task/required who))
    (let ([payload (async-channel-try-receive! ch)])
      (if payload
          (if (eq? (car payload) 'raise)
              (raise (cdr payload))
              (let ([values (cdr payload)])
                (if (cadr values)
                    (car values)
                    (raise (async-channel-closed-condition ch)))))
          (perform-operation (channel-get-operation ch))))))

(set-who! channel-receive
  (lambda (ch)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (async-check-operation-entry! (async-current-task/required who))
    (let ([payload (async-channel-try-receive! ch)])
      (if payload
          (async-return-payload payload)
          (perform-operation (channel-receive-operation ch))))))

(set! async-spawn-task
  (lambda (who thunk options termination-actions)
    (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
    (let ([name #f] [group #f] [context #f] [migratable? #f])
      (let loop ([opts options])
        (unless (null? opts)
          (unless (and (pair? (cdr opts)))
            ($oops who "odd number of option forms in ~s" options))
          (let ([k (car opts)] [v (cadr opts)])
            (cond
              [(eq? k 'name) (set! name v)]
              [(eq? k 'group)
               (unless (task-group? v) ($oops who "~s is not a task group" v))
               (set! group v)]
              [(eq? k 'context)
               (unless (async-context? v)
                 ($oops who "~s is not an async context" v))
               (set! context v)]
              [(eq? k 'migratable?) (set! migratable? (and v #t))]
              [else ($oops who "unrecognized spawn option ~s" k)]))
          (loop (cddr opts))))
      (let ([sched ($async-scheduler)])
        (unless (async-scheduler? sched)
          ($oops who "spawn-task outside of an async scheduler"))
        (unless (eq? (async-scheduler-status sched) 'running)
          ($oops who "scheduler is not running"))
        (let ([parent (async-scheduler-current-task sched)])
          (unless (or group parent)
            ($oops who "spawn-task requires a current task or an explicit group"))
          (let* ([grp (or group (ensure-child-group parent))]
                 [structural-parent
                  (and group parent (ensure-child-group parent))]
                 [parent-context
                  (or context
                      (and group (async-task-group-context group))
                      (async-current-context)
                      (async-task-context parent))]
                 [task (async-make-task sched name grp parent-context migratable?
                         termination-actions
                         (lambda () (run-task-entry thunk)))])
            (async-group-add-child! grp task structural-parent)
            (sched-registry-add! sched task)
            (if (and migratable?
                     (async-group-parallel? ($async-scheduler-group sched)))
                (async-work-submit! task sched)
                (begin
                  (async-debug-queue-claim! task 'next)
                  (async-queue-push! (async-scheduler-next-queue sched) task)))
            task))))))

(set-who! spawn-task
  (lambda (thunk . options)
    (async-spawn-task who thunk options '())))

(set-who! task-join
  (lambda (task)
    (unless (task? task) ($oops who "~s is not a task" task))
    (let ([current (async-current-task/required who)])
      (when (eq? task current)
        ($oops who "a task cannot join itself"))
      (async-check-operation-entry! current)
      (let ([payload (async-task-join-ready-payload! task)])
        (if payload
            (async-return-payload payload)
            (perform-operation (async-task-join-operation task)))))))

(set-who! task-join-operation
  (lambda (task)
    (unless (task? task) ($oops who "~s is not a task" task))
    (async-task-join-operation task)))

(set-who! task-cancel!
  (case-lambda
    [(task) (task-cancel! task #f)]
    [(task reason)
     (unless (task? task) ($oops who "~s is not a task" task))
     (let-values ([(child-group cancel-wait?)
                   (with-async-mutex (async-task-mutex task)
                     (if (or (task-terminal? task)
                             (task-cancel-requested? task))
                         (values #f #f)
                         (begin
                           (async-task-cancel-condition-set! task
                             (make-async-cancellation-condition reason))
                           (async-task-cancel-state-set! task 'requested)
                           (values
                             (async-task-child-group task)
                             (and (eq? (async-task-state task) 'waiting)
                                  (not (async-task-cancel-shield? task)))))))])
       (async-context-cancel-core! (async-task-context task) reason)
       (when child-group
         (group-cancel-children! child-group reason))
       (when cancel-wait?
         ($async-cancel-waiting-task task)))
     (void)]))

(set-who! task-yield
  (lambda ()
    (let ([sched ($async-scheduler)])
      (if (and (async-scheduler? sched)
               (async-scheduler-current-task sched))
          ($async-yield sched (async-scheduler-current-task sched))
          #f))))

(set-who! async-dynamic-wind
  (lambda (before thunk after)
    (unless (procedure? before) ($oops who "~s is not a procedure" before))
    (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
    (unless (procedure? after) ($oops who "~s is not a procedure" after))
    (dynamic-wind before thunk after)))

(set-who! run-async
  (lambda (thunk . options)
    (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
    (let ([clock 'real] [parallelism 1] [preemption-ticks #f])
      (let loop ([opts options])
        (unless (null? opts)
          (unless (and (pair? (cdr opts)))
            ($oops who "odd number of option forms in ~s" options))
          (let ([k (car opts)] [v (cadr opts)])
            (cond
              [(eq? k 'clock)
               (unless (memq v '(real virtual))
                 ($oops who "invalid clock ~s" v))
               (set! clock v)]
              [(eq? k 'parallelism)
               (unless (and (fixnum? v) (fx> v 0))
                 ($oops who "invalid parallelism ~s" v))
               (set! parallelism v)]
              [(eq? k 'preemption-ticks)
               (unless (or (not v)
                           (and (fixnum? v)
                                (fx>= v async-minimum-preemption-ticks)))
                 ($oops who "invalid preemption tick count ~s" v))
               (set! preemption-ticks v)]
              [else ($oops who "unrecognized run-async option ~s" k)]))
          (loop (cddr opts))))
      (let ([fiber ($current-native-fiber)])
        (when (and fiber
                   (fx= (fxlogand ($native-fiber-flags fiber)
                                  (constant native-fiber-flag-scheduler))
                        0))
          ($oops who "cannot nest an async scheduler inside an async task")))
      (when (and (eq? clock 'virtual) (fx> parallelism 1))
        ($oops who "parallel scheduler groups require a real clock"))
      (when (fx> parallelism 1)
        (if-feature pthreads
          (void)
          ($oops who "parallel scheduler groups require thread support")))
      (let* ([group (make-async-scheduler-group
                      (make-async-os-mutex)
                      (if-feature pthreads (make-condition) #f)
                      (make-async-queue)
                      (and async-debug-invariants? (make-eq-hashtable))
                      (box 0) '#() #f (box 0) #f (box 0) (box 0)
                      #f #f '())]
             [schedulers (make-vector parallelism)])
        (do ([i 0 (fx+ i 1)]) ((fx= i parallelism))
          (vector-set! schedulers i
            (async-make-scheduler
              (eq? clock 'virtual) group i preemption-ticks)))
        (async-scheduler-group-schedulers-set! group schedulers)
        (let* ([sched (vector-ref schedulers 0)]
               [root (async-make-task sched #f #f #f #f '()
                       (lambda () (run-task-entry thunk)))]
               [root-group
                (make-async-group #f (async-task-context root))])
        (async-scheduler-group-root-task-set! group root)
        (async-task-child-group-set! root root-group)
        (sched-registry-add! sched root)
        (async-debug-queue-claim! root 'next)
        (async-queue-push! (async-scheduler-next-queue sched) root)
        (let ([workers
               (if-feature pthreads
                 (let loop ([i 1] [workers '()])
                   (if (fx= i parallelism)
                       (reverse workers)
                       (let ([worker-sched (vector-ref schedulers i)])
                         (loop (fx+ i 1)
                           (cons (fork-thread
                                   (lambda ()
                                     (async-run-scheduler-thread worker-sched)))
                                 workers)))))
                 '())])
          (async-scheduler-group-workers-set! group workers)
          (let ([old-sched ($async-scheduler)])
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
                ($async-scheduler old-sched))))
          (async-group-shutdown! group)
          (if-feature pthreads
            (for-each thread-join workers)
            (void)))
        (async-debug-check-group-quiescent! group)
        (when (async-scheduler-group-failure group)
          (raise (async-scheduler-group-failure group)))
        (case (async-task-state root)
          [(completed) (apply values (async-task-result-values root))]
          [(failed) (raise (async-task-failure-condition root))]
          [(canceled) (raise (async-task-failure-condition root))]
          [else ($oops who "scheduler stopped with root task ~s"
                  (async-task-state root))]))))))

(set-who! current-async-task
  (lambda ()
    (let ([sched ($async-scheduler)])
      (and (async-scheduler? sched) (async-scheduler-current-task sched)))))

(set-who! current-async-scheduler
  (lambda ()
    (let ([sched ($async-scheduler)])
      (and (async-scheduler? sched) sched))))

(set-who! task-current-wait
  (lambda (task)
    (unless (task? task) ($oops who "~s is not a task" task))
    (async-task-current-wait task)))

(set-who! async-scheduler-task-count
  (lambda (sched)
    (unless (async-scheduler? sched) ($oops who "~s is not a scheduler" sched))
    (async-scheduler-group-task-count ($async-scheduler-group sched))))

(set-who! async-scheduler-turn-count
  (lambda (sched)
    (unless (async-scheduler? sched) ($oops who "~s is not a scheduler" sched))
    ($async-scheduler-turn-count sched)))

(set-who! async-scheduler-suspension-count
  (lambda (sched)
    (unless (async-scheduler? sched) ($oops who "~s is not a scheduler" sched))
    ($async-scheduler-suspension-count sched)))

(set-who! async-scheduler-wakeup-count
  (lambda (sched)
    (unless (async-scheduler? sched) ($oops who "~s is not a scheduler" sched))
    (async-atomic-box-ref (async-scheduler-wakeup-count-box sched))))

;;; ------------------------------------------------- io layer integration

;;; Hooks consumed by asyncio.ss.  $async-io-shutdown is replaced by the io
;;; layer's real shutdown procedure when that file is loaded.
(set! $async-pin-current-wait!
  (lambda ()
    (let* ([sched ($async-scheduler)]
           [task
            (and (async-scheduler? sched)
                 (async-scheduler-current-task sched))])
      (unless task
        ($oops '$async-pin-current-wait! "outside of an async task"))
      (with-async-mutex (async-task-mutex task)
        (async-task-resume-pinned?-set! task #t)))))
(set! $async-sync-state-live? async-sync-state-live?)
(set! $async-sync-slot-set! async-sync-slot-set!)
(set! $async-sync-slot-ref async-sync-slot-ref)
(set! $async-sync-slot-delete! async-sync-slot-delete!)

(set! $async-io-shutdown (lambda (sched) (void)))

(set! $async-scheduler-io-state
  (lambda (sched) (async-scheduler-io-state sched)))

(set! $async-scheduler-io-state-set!
  (lambda (sched v) (async-scheduler-io-state-set! sched v)))

(set! $async-scheduler-group-token
  (lambda (sched) ($async-scheduler-group sched)))

(set! $async-scheduler-owner-thread?
  (lambda (sched)
    (and (async-scheduler-owner-thread sched)
         (fx= (async-scheduler-owner-thread sched) (get-thread-id)))))

(set! $async-scheduler-poll-proc-set!
  (lambda (sched v) (async-scheduler-poll-proc-set! sched v)))

(set! $async-scheduler-wake-proc-set!
  (lambda (sched v) (async-scheduler-wake-proc-set! sched v)))

(set! $async-scheduler-timers
  (lambda (sched)
    (async-timer-heap-peek (async-scheduler-timers sched))))

(set! $async-scheduler-virtual?
  (lambda (sched) (async-scheduler-virtual? sched)))

(set! $async-timer-deadline
  (lambda (timer) (async-timer-deadline timer)))

(set! $async-monotonic-us
  (lambda () (async-monotonic-us)))

(set! $async-debug-invariants?
  (lambda () async-debug-invariants?))

(set! $async-debug-lock-rank-test async-debug-lock-rank-test)
(set! $async-debug-work-deque-model-test async-debug-work-deque-model-test)
(set! $async-debug-work-deque-contention-test
  async-debug-work-deque-contention-test)
