;;; -------------------------------------------------------------- suspension

(define async-suspend-token (list 'async-suspended))
(define async-yield-token (list 'async-yielded))

;;; Cooperative yield is a scheduler transition, not a cancelable wait.  It
;;; therefore bypasses the generic operation registration handshake while
;;; retaining cancellation points on both sides of the transfer.
(define $async-yield
  (lambda (sched task)
    (async-check-operation-entry! task)
    ($async-scheduler-suspension-count-set! sched
      (fx+ 1 ($async-scheduler-suspension-count sched)))
    (let ([scheduler-fiber (async-scheduler-native-fiber sched)]
          [task-fiber (async-task-native-fiber task)])
      (async-debug-check-native-fiber! '$async-yield
        task-fiber '(running) #f)
      (async-debug-check-native-fiber! '$async-yield
        scheduler-fiber '(parked) #t)
      (unless ($native-fiber-try-claim! scheduler-fiber)
        ($oops '$async-yield "scheduler fiber is not claimable"))
      (guard (c [else
                 ($native-fiber-release-claim! scheduler-fiber)
                 (raise c)])
        ($native-fiber-switch task-fiber scheduler-fiber async-yield-token))
      (async-check-operation-entry! task)
      #t)))

;;; Publish one checked wait and park the active native task fiber.  Completion
;;; may win while registration is in progress; the suspension-state handshake
;;; defers runnable publication until the scheduler observes the parked fiber.
(define $async-suspend
  (lambda (sched task ss register!)
    (async-check-cancellation! task)
    (with-async-mutex (async-task-mutex task)
      (async-task-state-set! task 'waiting)
      (async-task-wait-scheduler-set! task sched)
      (async-task-suspension-state-set! task 'unwinding)
      (async-task-sync-state-set! task ss)
      ($async-scheduler-suspension-count-set! sched
        (fx+ 1 ($async-scheduler-suspension-count sched))))
    (let* ([desc (register! ss)]
           [cancel?
            (with-async-mutex (async-task-mutex task)
              (when (and desc
                         (eq? (async-task-state task) 'waiting)
                         (async-sync-state-live? ss))
                (async-task-current-wait-set! task desc))
              (and (async-task-cancel-state task)
                   (eq? (async-task-state task) 'waiting)
                   (not (async-task-cancel-shield? task))))])
      (when cancel?
        ($async-cancel-waiting-task task)))
    (let ([scheduler-fiber (async-scheduler-native-fiber sched)]
          [task-fiber (async-task-native-fiber task)])
      (async-debug-check-native-fiber! '$async-suspend task-fiber '(running) #f)
      (async-debug-check-native-fiber! '$async-suspend scheduler-fiber '(parked) #t)
      (unless ($native-fiber-try-claim! scheduler-fiber)
        ($oops '$async-suspend "scheduler fiber is not claimable"))
      (let ([payload
             (guard (c [else
                        ($native-fiber-release-claim! scheduler-fiber)
                        (raise c)])
               ($native-fiber-switch task-fiber scheduler-fiber
                 async-suspend-token))])
        (async-debug-check-native-fiber! '$async-suspend task-fiber '(running) #f)
        ;; A migratable task can resume on another worker; the scheduler that
        ;; accepted this suspension is then foreign-owned and may be in a
        ;; transient handoff.  Its next local stable boundary validates it.
        (async-check-cancellation! task)
        payload))))

(define async-deliver-operation-payload
  (lambda (op payload)
    (if (eq? (car payload) 'values)
        (apply values ((operation-wrap op) (cdr payload)))
        (raise (cdr payload)))))

(define async-deliver-operation-result
  (lambda (op payload)
    (async-deliver-operation-payload op payload)))

;;; --------------------------------------------------------------- operations







;;; ------------------------------------------------------------------ timers

(define async-timer-before?
  (lambda (a b)
    (or (< (async-timer-deadline a) (async-timer-deadline b))
        (and (= (async-timer-deadline a) (async-timer-deadline b))
             (< (async-timer-sequence a) (async-timer-sequence b))))))

(define async-timer-heap-swap!
  (lambda (heap i j)
    (let* ([items (async-timer-heap-items heap)]
           [a (vector-ref items i)]
           [b (vector-ref items j)])
      (vector-set! items i b)
      (vector-set! items j a)
      (async-timer-index-set! a j)
      (async-timer-index-set! b i))))

(define async-timer-heap-sift-up!
  (lambda (heap index)
    (let loop ([i index])
      (unless (fx= i 0)
        (let* ([parent (fxquotient (fx- i 1) 2)]
               [items (async-timer-heap-items heap)])
          (when (async-timer-before?
                  (vector-ref items i) (vector-ref items parent))
            (async-timer-heap-swap! heap i parent)
            (loop parent)))))))

(define async-timer-heap-sift-down!
  (lambda (heap index)
    (let ([size (async-timer-heap-size heap)])
      (let loop ([i index])
        (let ([left (fx+ (fx* i 2) 1)])
          (when (fx< left size)
            (let* ([right (fx+ left 1)]
                   [items (async-timer-heap-items heap)]
                   [child
                    (if (and (fx< right size)
                             (async-timer-before?
                               (vector-ref items right)
                               (vector-ref items left)))
                        right
                        left)])
              (when (async-timer-before?
                      (vector-ref items child) (vector-ref items i))
                (async-timer-heap-swap! heap i child)
                (loop child)))))))))

(define async-timer-heap-grow!
  (lambda (heap)
    (let* ([old (async-timer-heap-items heap)]
           [new (make-vector (fx* 2 (vector-length old)) #f)])
      (vector-copy! old 0 new 0 (vector-length old))
      (async-timer-heap-items-set! heap new))))

(define async-timer-heap-push/raw!
  (lambda (heap timer)
    (let ([size (async-timer-heap-size heap)])
      (when (fx= size (vector-length (async-timer-heap-items heap)))
        (async-timer-heap-grow! heap))
      (vector-set! (async-timer-heap-items heap) size timer)
      (async-timer-heap-size-set! heap (fx+ size 1))
      (async-timer-owner-heap-set! timer heap)
      (async-timer-index-set! timer size)
      (async-timer-heap-sift-up! heap size))))

(define async-timer-heap-remove/raw!
  (lambda (heap timer)
    (let ([index (async-timer-index timer)]
          [size (async-timer-heap-size heap)])
      (when (and (async-timer-owner-heap timer)
                 (eq? (async-timer-owner-heap timer) heap)
                 (fx>= index 0)
                 (fx< index size)
                 (eq? (vector-ref (async-timer-heap-items heap) index) timer))
        (let* ([items (async-timer-heap-items heap)]
               [last-index (fx- size 1)]
               [last (vector-ref items last-index)])
          (vector-set! items last-index #f)
          (async-timer-heap-size-set! heap last-index)
          (async-timer-owner-heap-set! timer #f)
          (async-timer-index-set! timer -1)
          (unless (fx= index last-index)
            (vector-set! items index last)
            (async-timer-index-set! last index)
            (if (and (fx> index 0)
                     (async-timer-before? last
                       (vector-ref items (fxquotient (fx- index 1) 2))))
                (async-timer-heap-sift-up! heap index)
                (async-timer-heap-sift-down! heap index)))
          #t)))))

(define async-timer-heap-peek
  (lambda (heap)
    (with-async-mutex (async-timer-heap-mutex heap)
      (and (fx> (async-timer-heap-size heap) 0)
           (vector-ref (async-timer-heap-items heap) 0)))))

(define async-timer-heap-pop-due!
  (lambda (heap now)
    (with-async-mutex (async-timer-heap-mutex heap)
      (let ([timer
             (and (fx> (async-timer-heap-size heap) 0)
                  (vector-ref (async-timer-heap-items heap) 0))])
        (and timer
             (<= (async-timer-deadline timer) now)
             (begin
               (async-timer-heap-remove/raw! heap timer)
               timer))))))

(define async-schedule-timer!
  (lambda (sched deadline deliver)
    (let ([heap (async-scheduler-timers sched)])
      (with-async-mutex (async-timer-heap-mutex heap)
        (let* ([sequence (async-timer-heap-next-sequence heap)]
               [timer (make-async-timer deadline sequence (box deliver) #f -1)])
          (async-timer-heap-next-sequence-set! heap (fx+ sequence 1))
          (async-timer-heap-push/raw! heap timer)
          timer)))))

(define async-fire-due-timers!
  (lambda (sched)
    (let* ([heap (async-scheduler-timers sched)]
           [next (async-timer-heap-peek heap)])
      ;; Querying the platform monotonic clock is materially more expensive
      ;; than an empty-heap check and should disappear from timer-free turns.
      (when next
        (let ([now (sched-now sched)])
          (let loop ()
            (let ([timer (async-timer-heap-pop-due! heap now)])
              (when timer
                (let* ([deliver-box (async-timer-deliver-box timer)]
                       [deliver (unbox deliver-box)])
                  (when (and deliver (box-cas! deliver-box deliver #f))
                    (deliver (cons 'values '()))))
                (loop)))))))))



;;; ----------------------------------------------------------------- futures


(define future-complete!
  (lambda (f payload)
    (let ([waiters
           (with-async-mutex (async-future-mutex f)
             (unless (box-cas! (async-future-state f) 'waiting 'claimed)
               ($oops 'future-fulfil! "future is already fulfilled"))
             (let ([waiters
                    (async-wait-queue-drain! (async-future-waiters f))])
               (async-atomic-box-set!
                 (async-future-state f) (cons 'done payload))
               waiters))])
      (for-each (lambda (w) ((cdr w) payload)) waiters))))

(define future-ready-payload
  (lambda (f)
    (let ([state (async-atomic-box-ref (async-future-state f))])
      (and (pair? state) (eq? (car state) 'done) (cdr state)))))





;;; ------------------------------------------------------------- async mutexes

(define async-mutex-current-task
  (lambda (who)
    (async-current-task/required who)))

(define async-owned-mutex-list-limit 8)

(define async-task-owned-mutex-list
  (lambda (task)
    (let ([owned (async-task-owned-mutexes task)])
      (if (hashtable? owned)
          (vector->list (hashtable-keys owned))
          owned))))

(define async-task-add-owned-mutex!
  (lambda (task mutex)
    (with-async-mutex (async-task-mutex task)
      (let ([owned (async-task-owned-mutexes task)])
        (if (hashtable? owned)
            (hashtable-set! owned mutex mutex)
            (unless (memq mutex owned)
              (if (fx>= (length owned) async-owned-mutex-list-limit)
                  (let ([table (make-eq-hashtable)])
                    (for-each
                      (lambda (lock) (hashtable-set! table lock lock))
                      owned)
                    (hashtable-set! table mutex mutex)
                    (async-task-owned-mutexes-set! task table))
                  (async-task-owned-mutexes-set! task
                    (cons mutex owned)))))))))

(define async-task-remove-owned-mutex!
  (lambda (task mutex)
    (with-async-mutex (async-task-mutex task)
      (let ([owned (async-task-owned-mutexes task)])
        (if (hashtable? owned)
            (hashtable-delete! owned mutex)
            (async-task-owned-mutexes-set! task
              (remq mutex owned)))))))

;;; Ownership is published before a waiter can resume on another scheduler.
(define async-mutex-handoff!
  (lambda (mutex)
    (let loop ()
      (let ([node (async-wait-queue-pop!
                    (async-fiber-mutex-waiters mutex))])
        (if (not node)
            (begin (async-fiber-mutex-owner-set! mutex #f) #f)
            (let* ([waiter (async-wait-node-value node)]
                   [task (car waiter)]
                   [ss (cadr waiter)]
                   [deliver (cddr waiter)])
              (let ([reservation
                     (and (async-sync-state-live? ss)
                          (deliver 'reserve (cons 'values '())))])
                (if reservation
                    (let ([publish
                           (async-delivery-prepare! reservation)])
                      (async-fiber-mutex-owner-set! mutex task)
                      (lambda ()
                        (async-task-add-owned-mutex! task mutex)
                        (publish)))
                    (loop)))))))))

(define async-fiber-mutex-try-acquire!
  (lambda (mutex task who)
    (let ([acquired?
           (with-async-mutex (async-fiber-mutex-mutex mutex)
             (cond
               [(not (async-fiber-mutex-owner mutex))
                (async-fiber-mutex-owner-set! mutex task)
                (async-task-add-owned-mutex! task mutex)
                #t]
               [(eq? (async-fiber-mutex-owner mutex) task)
                ($oops who "mutex is not recursive")]
               [else #f]))])
      acquired?)))

(define async-fiber-mutex-acquire-operation
  (lambda (mutex)
    (let ([token (list 'async-mutex-acquire-operation)]
          [node-token (list 'async-mutex-acquire-waiter)])
      (make-async-operation
        (lambda (ss)
          (let ([task (async-mutex-current-task
                        'async-mutex-acquire-operation)])
            (if (async-fiber-mutex-try-acquire! mutex task
                  'async-mutex-acquire-operation)
                (cons 'values '())
                (begin
                  (async-sync-slot-set! ss token task)
                  #f))))
        (lambda (ss deliver)
          (let ([task (async-sync-slot-ref ss token #f)])
            (unless task
              ($oops 'async-mutex-acquire-operation
                "acquire operation has no current task"))
            (let-values ([(descriptor action)
                          (with-async-mutex
                            (async-fiber-mutex-mutex mutex)
                            (cond
                              [(not (async-fiber-mutex-owner mutex))
                               (let ([reservation
                                      (deliver 'reserve (cons 'values '()))])
                                 (if reservation
                                     (let ([publish
                                            (async-delivery-prepare!
                                              reservation)])
                                       (async-fiber-mutex-owner-set! mutex task)
                                       (values #f
                                         (lambda ()
                                           (async-task-add-owned-mutex!
                                             task mutex)
                                           (publish))))
                                     (values #f (async-mutex-handoff! mutex))))]
                              [(eq? (async-fiber-mutex-owner mutex) task)
                               ($oops 'async-mutex-acquire-operation
                                 "mutex is not recursive")]
                              [else
                               (async-sync-slot-set! ss node-token
                                 (async-wait-queue-add!
                                   (async-fiber-mutex-waiters mutex)
                                   (cons task (cons ss deliver))))
                               (values (list 'async-mutex) #f)]))])
              (when action (action))
              descriptor)))
        (lambda (values) values)
        (lambda (ss)
          (with-async-mutex (async-fiber-mutex-mutex mutex)
            (let ([node (async-sync-slot-ref ss node-token #f)])
              (when node
                (async-wait-queue-remove!
                  (async-fiber-mutex-waiters mutex) node)
                (async-sync-slot-delete! ss node-token)))))))))


;;; ----------------------------------------------------------- read/write mutexes

(define async-rw-mutex-reader-count
  (lambda (mutex task)
    (hashtable-ref (async-rw-mutex-readers mutex) task 0)))

(define async-rw-mutex-add-reader!
  (lambda (mutex task)
    (let ([readers (async-rw-mutex-readers mutex)])
      (hashtable-set! readers task
        (fx+ 1 (hashtable-ref readers task 0)))
      (async-task-add-owned-mutex! task mutex))))

(define async-rw-mutex-remove-reader!
  (lambda (mutex task)
    (let* ([readers (async-rw-mutex-readers mutex)]
           [count (hashtable-ref readers task 0)])
      (cond
        [(fx= count 0) #f]
        [(fx= count 1)
         (hashtable-delete! readers task)
         (async-task-remove-owned-mutex! task mutex)
         #t]
        [else
         (hashtable-set! readers task (fx- count 1))
         #t]))))

(define async-rw-mutex-idle?
  (lambda (mutex)
    (and (not (async-rw-mutex-writer mutex))
         (fx= (hashtable-size (async-rw-mutex-readers mutex)) 0))))

(define async-rw-mutex-try-acquire!
  (lambda (mutex mode task)
    (with-async-mutex (async-rw-mutex-mutex mutex)
      (if (eq? mode 'read)
          (cond
            [(eq? (async-rw-mutex-writer mutex) task)
             ($oops 'async-rw-mutex-read-acquire
               "write owner cannot acquire a read lock")]
            [(fx> (async-rw-mutex-reader-count mutex task) 0)
             (async-rw-mutex-add-reader! mutex task)
             #t]
            [(and (not (async-rw-mutex-writer mutex))
                  (async-wait-queue-empty? (async-rw-mutex-waiters mutex)))
             (async-rw-mutex-add-reader! mutex task)
             #t]
            [else #f])
          (cond
            [(eq? (async-rw-mutex-writer mutex) task)
             ($oops 'async-rw-mutex-acquire "mutex is not recursive")]
            [(fx> (async-rw-mutex-reader-count mutex task) 0)
             ($oops 'async-rw-mutex-acquire
               "read-to-write upgrade is not supported")]
            [(and (async-rw-mutex-idle? mutex)
                  (async-wait-queue-empty? (async-rw-mutex-waiters mutex)))
             (async-rw-mutex-writer-set! mutex task)
             (async-task-add-owned-mutex! task mutex)
             #t]
            [else #f])))))

;;; Grant either the first writer or the consecutive reader prefix.  The
;;; state is installed before delivery so resumed tasks observe ownership.
(define async-rw-mutex-handoff!
  (lambda (mutex)
    (let writer-loop ()
      (let ([node (async-wait-queue-pop! (async-rw-mutex-waiters mutex))])
        (if node
            (let ([waiter (async-wait-node-value node)])
              (cond
              [(not (async-sync-state-live? (vector-ref waiter 2)))
               (writer-loop)]
              [(eq? (vector-ref waiter 0) 'write)
               (let ([task (vector-ref waiter 1)]
                     [deliver (vector-ref waiter 3)])
                 (let ([reservation
                        (deliver 'reserve (cons 'values '()))])
                   (if reservation
                       (let ([publish
                              (async-delivery-prepare! reservation)])
                         (async-rw-mutex-writer-set! mutex task)
                         (async-task-add-owned-mutex! task mutex)
                         publish)
                       (writer-loop))))]
              [else
               (let reader-loop ([reader waiter] [publications '()])
                 (let* ([task (vector-ref reader 1)]
                        [ss (vector-ref reader 2)]
                        [deliver (vector-ref reader 3)]
                        [reservation
                         (and (async-sync-state-live? ss)
                              (deliver 'reserve (cons 'values '())))]
                        [publications
                         (if reservation
                             (begin
                               (async-rw-mutex-add-reader! mutex task)
                               (cons (async-delivery-prepare! reservation)
                                 publications))
                             publications)]
                        [next
                         (async-wait-queue-head
                           (async-rw-mutex-waiters mutex))])
                   (if (and next
                            (eq? (vector-ref
                                   (async-wait-node-value next) 0)
                                 'read))
                       (reader-loop
                         (async-wait-node-value
                           (async-wait-queue-pop!
                             (async-rw-mutex-waiters mutex)))
                         publications)
                       (if (null? publications)
                           (writer-loop)
                           (lambda ()
                             (async-delivery-publish-all!
                               (reverse publications)))))))]))
            #f)))))

(define async-rw-mutex-acquire-operation/raw
  (lambda (mutex mode)
    (let ([token (list 'async-rw-mutex-acquire-operation mode)]
          [node-token (list 'async-rw-mutex-acquire-waiter mode)])
      (make-async-operation
        (lambda (ss)
          (let ([task (async-mutex-current-task
                        (if (eq? mode 'read)
                            'async-rw-mutex-read-acquire-operation
                            'async-rw-mutex-acquire-operation))])
            (if (async-rw-mutex-try-acquire! mutex mode task)
                (cons 'values '())
                (begin
                  (async-sync-slot-set! ss token task)
                  #f))))
        (lambda (ss deliver)
          (let ([task (async-sync-slot-ref ss token #f)])
            (unless task
              ($oops 'async-rw-mutex-acquire-operation
                "acquire operation has no current task"))
            (let-values ([(descriptor action)
                          (with-async-mutex (async-rw-mutex-mutex mutex)
                            (cond
                              [(and (eq? mode 'read)
                                    (not (async-rw-mutex-writer mutex))
                                    (async-wait-queue-empty?
                                      (async-rw-mutex-waiters mutex)))
                               (let ([reservation
                                      (deliver 'reserve (cons 'values '()))])
                                 (if reservation
                                     (begin
                                       (async-rw-mutex-add-reader! mutex task)
                                       (values #f
                                         (async-delivery-prepare! reservation)))
                                     (values #f
                                       (and (async-rw-mutex-idle? mutex)
                                            (async-rw-mutex-handoff! mutex)))))]
                              [(and (eq? mode 'write)
                                    (async-rw-mutex-idle? mutex)
                                    (async-wait-queue-empty?
                                      (async-rw-mutex-waiters mutex)))
                               (let ([reservation
                                      (deliver 'reserve (cons 'values '()))])
                                 (if reservation
                                     (begin
                                       (async-rw-mutex-writer-set! mutex task)
                                       (async-task-add-owned-mutex! task mutex)
                                       (values #f
                                         (async-delivery-prepare! reservation)))
                                     (values #f
                                       (async-rw-mutex-handoff! mutex))))]
                              [else
                               (async-sync-slot-set! ss node-token
                                 (async-wait-queue-add!
                                   (async-rw-mutex-waiters mutex)
                                   (vector mode task ss deliver)))
                               (values (list 'async-rw-mutex mode) #f)]))])
              (when action (action))
              descriptor)))
        (lambda (values) values)
        (lambda (ss)
          (with-async-mutex (async-rw-mutex-mutex mutex)
            (let ([node (async-sync-slot-ref ss node-token #f)])
              (when node
                (async-wait-queue-remove! (async-rw-mutex-waiters mutex) node)
                (async-sync-slot-delete! ss node-token)))))))))


;;; ------------------------------------------------ wait groups, once, conditions

(define async-wait-group-wait-operation/raw
  (lambda (group)
    (let ([token (list 'async-wait-group-wait-operation)])
      (make-async-operation
      (lambda (ss)
        (with-async-mutex (async-wait-group-mutex group)
          (and (= (async-wait-group-count group) 0)
               (cons 'values '()))))
      (lambda (ss deliver)
        (let ([blocked?
               (with-async-mutex (async-wait-group-mutex group)
                 (if (= (async-wait-group-count group) 0)
                     #f
                     (begin
                       (async-sync-slot-set! ss token
                         (async-wait-queue-add!
                           (async-wait-group-waiters group)
                           (cons ss deliver)))
                       #t)))])
          (if blocked?
              (list 'async-wait-group)
              (begin (deliver (cons 'values '())) #f))))
      (lambda (values) values)
      (lambda (ss)
        (with-async-mutex (async-wait-group-mutex group)
          (let ([node (async-sync-slot-ref ss token #f)])
            (when node
              (async-wait-queue-remove! (async-wait-group-waiters group) node)
              (async-sync-slot-delete! ss token)))))))))

(define async-once-wait-operation
  (lambda (once)
    (let ([token (list 'async-once-wait-operation)])
      (make-async-operation
      (lambda (ss)
        (with-async-mutex (async-once-mutex once)
          (and (eq? (async-once-state once) 'done)
               (cons 'values '()))))
      (lambda (ss deliver)
        (let ([blocked?
               (with-async-mutex (async-once-mutex once)
                 (if (eq? (async-once-state once) 'done)
                     #f
                     (begin
                       (async-sync-slot-set! ss token
                         (async-wait-queue-add! (async-once-waiters once)
                           (cons ss deliver)))
                       #t)))])
          (if blocked?
              (list 'async-once)
              (begin (deliver (cons 'values '())) #f))))
      (lambda (values) values)
      (lambda (ss)
        (with-async-mutex (async-once-mutex once)
          (let ([node (async-sync-slot-ref ss token #f)])
            (when node
              (async-wait-queue-remove! (async-once-waiters once) node)
              (async-sync-slot-delete! ss token)))))))))

(define async-lock-owned-by?
  (lambda (lock task)
    (cond
      [($async-mutex? lock)
       (with-async-mutex (async-fiber-mutex-mutex lock)
         (eq? (async-fiber-mutex-owner lock) task))]
      [($async-rw-mutex? lock)
       (with-async-mutex (async-rw-mutex-mutex lock)
         (eq? (async-rw-mutex-writer lock) task))]
      [else #f])))

(define async-lock-release!
  (lambda (lock)
    (if ($async-mutex? lock)
        (async-mutex-release! lock)
        (async-rw-mutex-release! lock))))

(define async-lock-acquire!
  (lambda (lock)
    (if ($async-mutex? lock)
        (async-mutex-acquire lock)
        (async-rw-mutex-acquire lock))))

(define async-condition-wait-operation
  (lambda (condition lock task released-box)
    (let ([token (list 'async-condition-wait-operation)])
      (make-async-operation
      (lambda (ss) #f)
      (lambda (ss deliver)
        (with-async-mutex (async-condition-guard condition)
          (let ([shielded-deliver
                 (case-lambda
                   [(payload)
                    ;; Publish the shield before making the task runnable, so
                    ;; cancellation cannot overtake lock reacquisition.
                    (async-task-cancel-shield?-set! task #t)
                    (deliver payload)]
                   [(command payload)
                    (let ([reservation (deliver command payload)])
                      (when reservation
                        (async-delivery-add-action! reservation
                          (lambda ()
                            (async-task-cancel-shield?-set! task #t))))
                      reservation)])])
            (async-sync-slot-set! ss token
              (async-wait-queue-add! (async-condition-waiters condition)
                (cons ss shielded-deliver))))
          ;; Registration precedes unlock. A conventional signaler that owns
          ;; the same lock cannot pass this point early and lose the wakeup.
          (async-lock-release! lock)
          (set-box! released-box #t)
          (list 'async-condition)))
      (lambda (values) values)
      (lambda (ss)
        (with-async-mutex (async-condition-guard condition)
          (let ([node (async-sync-slot-ref ss token #f)])
            (when node
              (async-wait-queue-remove! (async-condition-waiters condition) node)
              (async-sync-slot-delete! ss token)))
          ;; The only chooser around this private operation is the current
          ;; cancellation context. Shield before its cancellation payload can
          ;; resume the task.
          (when (unbox released-box)
            (async-task-cancel-shield?-set! task #t))))))))

(define async-release-task-lock!
  (lambda (lock task)
    (cond
      [($async-mutex? lock)
       (let ([action
              (with-async-mutex (async-fiber-mutex-mutex lock)
                (and (eq? (async-fiber-mutex-owner lock) task)
                     (begin
                       (async-fiber-mutex-owner-set! lock #f)
                       (async-mutex-handoff! lock))))])
         (async-task-remove-owned-mutex! task lock)
         (when action (action)))]
      [($async-rw-mutex? lock)
       (let ([action
              (with-async-mutex (async-rw-mutex-mutex lock)
                (when (eq? (async-rw-mutex-writer lock) task)
                  (async-rw-mutex-writer-set! lock #f))
                (when (fx> (async-rw-mutex-reader-count lock task) 0)
                  (hashtable-delete! (async-rw-mutex-readers lock) task))
                (and (async-rw-mutex-idle? lock)
                     (async-rw-mutex-handoff! lock)))])
         (async-task-remove-owned-mutex! task lock)
         (when action (action)))]
      [else (void)])))

(define async-call-with-rw-lock
  (lambda (mutex thunk read?)
    (unless ($async-rw-mutex? mutex)
      ($oops (if read? 'call-with-async-read-mutex
                 'call-with-async-rw-mutex)
        "~s is not an async rw mutex" mutex))
    (unless (procedure? thunk)
      ($oops (if read? 'call-with-async-read-mutex
                 'call-with-async-rw-mutex)
        "~s is not a procedure" thunk))
    ((if read? async-rw-mutex-read-acquire async-rw-mutex-acquire) mutex)
    (let ([released? #f])
      (define release!
        (lambda ()
          (unless released?
            (set! released? #t)
            ((if read? async-rw-mutex-read-release!
                 async-rw-mutex-release!) mutex))))
      (guard (condition
               [else (release!) (raise condition)])
        (call-with-values thunk
          (lambda result*
            (release!)
            (apply values result*)))))))


;;; ---------------------------------------------------------------- channels


(define async-waiter-dead?
  (lambda (ss) (not (async-sync-state-live? ss))))

(define async-channel-closed-condition
  (lambda (ch)
    ($make-channel-closed-condition (async-channel-close-reason ch))))

(define async-channel-put-closed-payload
  (lambda (ch)
    (cons 'raise (async-channel-closed-condition ch))))

(define async-channel-receive-closed-payload
  (lambda () (cons 'values '(#f #f))))

;;; Reserve the first live getter.  The caller prepares the reservation while
;;; holding the channel mutex and publishes it after releasing the mutex.
(define async-channel-reserve-getter!
  (lambda (ch v)
    (let loop ()
      (let ([node (async-wait-queue-pop! (async-channel-gets ch))])
        (and node
             (let ([waiter (async-wait-node-value node)])
               (let ([reservation
                      (and (not (async-waiter-dead? (car waiter)))
                           ((cdr waiter) 'reserve
                             (cons 'values (list v #t))))])
                 (if reservation
                   (cons node reservation)
                   (loop)))))))))

;;; Reserve the first live putter and return (value . reservation).
(define async-channel-reserve-putter!
  (lambda (ch)
    (let loop ()
      (let ([node (async-wait-queue-pop! (async-channel-puts ch))])
        (and node
             (let ([waiter (async-wait-node-value node)])
               (let ([reservation
                      (and (not (async-waiter-dead? (cadr waiter)))
                           ((caddr waiter) 'reserve (cons 'values '())))])
                 (if reservation
                     (vector (car waiter) node reservation)
                     (loop)))))))))

(define async-buffer-push!
  (lambda (ch v)
    (let ([buf (async-channel-buffer ch)]
          [cap (async-channel-capacity ch)]
          [n (async-channel-bcount ch)])
      (vector-set! buf (fxmod (fx+ (async-channel-bstart ch) n) cap) v)
      (async-channel-bcount-set! ch (fx+ n 1)))))

(define async-buffer-pop!
  (lambda (ch)
    (let ([buf (async-channel-buffer ch)]
          [cap (async-channel-capacity ch)]
          [start (async-channel-bstart ch)])
      (let ([v (vector-ref buf start)])
        (vector-set! buf start #f)
        (async-channel-bstart-set! ch (fxmod (fx+ start 1) cap))
        (async-channel-bcount-set! ch (fx- (async-channel-bcount ch) 1))
        v))))
