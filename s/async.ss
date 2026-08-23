;;; async.ss
;;; Copyright 2026 Cisco Systems, Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Fiber-based asynchronous execution facility (ASYNC.md).
;;;
;;; A scheduler establishes a private continuation prompt for each task turn
;;; and runs lightweight tasks on one operating-system thread.  Tasks suspend
;;; with one-shot delimited continuations.  Waitables (timers, channels,
;;; futures, joins) are expressed as operations with try/block/wrap/nack
;;; components.  Optional tick preemption captures the same one-shot fiber
;;; continuation directly at a runtime safe point.  libuv-backed I/O plugs in
;;; through the scheduler's io fields (asyncio.ss).

;;; ----------------------------------------------------------- utilities

(let ()  ; private scope: public names are assigned to their declared globals

(define-syntax with-async-mutex
  (lambda (x)
    (syntax-case x ()
      [(_ m e1 e2 ...)
       (if-feature pthreads
         #'(critical-section (with-mutex m e1 e2 ...))
         #'(begin e1 e2 ...))])))

(define make-async-os-mutex
  (lambda () (if-feature pthreads (make-mutex) #f)))

(define async-debug-invariants?
  (let ([v (getenv "CHEZ_ASYNC_CHECK_INVARIANTS")])
    (and v (not (member v '("" "0" "false" "no"))))))

(define async-debug-runnable-mutex (make-async-os-mutex))
(define async-debug-runnable (make-eq-hashtable))

(define async-invariant
  (lambda (ok? message object)
    (when (and async-debug-invariants? (not ok?))
      ($oops 'async-invariant "~a: ~s" message object))))

(define async-debug-queue-claim!
  (lambda (task location)
    (when async-debug-invariants?
      (with-async-mutex async-debug-runnable-mutex
        (async-invariant
          (not (hashtable-ref async-debug-runnable task #f))
          "task is present in more than one runnable queue" task)
        (async-invariant (eq? (async-task-state task) 'ready)
          "queued task is not ready" task)
        (hashtable-set! async-debug-runnable task location)))))

(define async-debug-queue-release!
  (lambda (task)
    (when async-debug-invariants?
      (with-async-mutex async-debug-runnable-mutex
        (async-invariant (hashtable-ref async-debug-runnable task #f)
          "dequeued task was not registered as runnable" task)
        (hashtable-delete! async-debug-runnable task)))))

(define-record-type (async-fifo make-async-fifo async-fifo?)
  (nongenerative)
  (sealed #t)
  (fields (mutable head) (mutable tail)))

(define make-async-queue
  (lambda () (make-async-fifo '() '())))

(define async-queue-empty?
  (lambda (q) (null? (async-fifo-head q))))

(define async-queue-push!
  (lambda (q x)
    (let ([p (cons x '())])
      (if (null? (async-fifo-head q))
          (async-fifo-head-set! q p)
          (set-cdr! (async-fifo-tail q) p))
      (async-fifo-tail-set! q p))))

(define async-queue-pop!
  (lambda (q)
    (let ([h (async-fifo-head q)])
      (async-fifo-head-set! q (cdr h))
      (when (null? (cdr h)) (async-fifo-tail-set! q '()))
      (car h))))

;;; Intrusive FIFO for cancelable waiters.  A registration retains its node,
;;; so cancellation can unlink it in O(1) without rebuilding the queue.
(define-record-type (async-wait-node make-async-wait-node async-wait-node?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable value)
    (mutable queue)
    (mutable previous)
    (mutable next)))

(define-record-type (async-wait-queue make-async-wait-queue% async-wait-queue?)
  (nongenerative)
  (sealed #t)
  (fields (mutable head) (mutable tail)))

(define make-async-wait-queue
  (lambda () (make-async-wait-queue% #f #f)))

(define async-wait-queue-empty?
  (lambda (queue) (not (async-wait-queue-head queue))))

(define async-wait-queue-add!
  (lambda (queue value)
    (let* ([tail (async-wait-queue-tail queue)]
           [node (make-async-wait-node value queue tail #f)])
      (if tail
          (async-wait-node-next-set! tail node)
          (async-wait-queue-head-set! queue node))
      (async-wait-queue-tail-set! queue node)
      node)))

(define async-wait-queue-remove!
  (lambda (queue node)
    (and (async-wait-node? node)
         (eq? (async-wait-node-queue node) queue)
         (let ([previous (async-wait-node-previous node)]
               [next (async-wait-node-next node)])
           (if previous
               (async-wait-node-next-set! previous next)
               (async-wait-queue-head-set! queue next))
           (if next
               (async-wait-node-previous-set! next previous)
               (async-wait-queue-tail-set! queue previous))
           (async-wait-node-queue-set! node #f)
           (async-wait-node-previous-set! node #f)
           (async-wait-node-next-set! node #f)
           #t))))

(define async-wait-queue-reinsert-front!
  (lambda (queue node)
    (and (not (async-wait-node-queue node))
         (let ([head (async-wait-queue-head queue)])
           (async-wait-node-queue-set! node queue)
           (async-wait-node-previous-set! node #f)
           (async-wait-node-next-set! node head)
           (when head (async-wait-node-previous-set! head node))
           (async-wait-queue-head-set! queue node)
           (unless (async-wait-queue-tail queue)
             (async-wait-queue-tail-set! queue node))
           #t))))

(define async-wait-queue-pop!
  (lambda (queue)
    (let ([node (async-wait-queue-head queue)])
      (when node (async-wait-queue-remove! queue node))
      node)))

(define async-wait-queue-drain!
  (lambda (queue)
    (let loop ([values '()])
      (let ([node (async-wait-queue-pop! queue)])
        (if node
            (loop (cons (async-wait-node-value node) values))
            (reverse values))))))

;;; Owner-only Chase--Lev work-stealing deque.  The owner publishes and
;;; removes work at bottom; thieves compete for top with CAS.  Growing
;;; publishes a fresh ring while the old ring remains reachable by any thief
;;; that already loaded it.
(define-record-type (async-work-ring make-async-work-ring async-work-ring?)
  (nongenerative)
  (sealed #t)
  (fields (immutable slots) (immutable mask)))

(define-record-type (async-work-deque make-async-work-deque% async-work-deque?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable top)                 ; atomic box
    (immutable bottom)              ; atomic box; written only by owner
    (immutable ring)))              ; atomic box of async-work-ring

(define make-async-work-deque
  (lambda ()
    (let ([slots (make-vector 32 #f)])
      (make-async-work-deque%
        (box 0) (box 0)
        (box (make-async-work-ring slots (fx- (vector-length slots) 1)))))))

(define async-atomic-box-set!
  (lambda (b new)
    (let loop ([old (unbox b)])
      (unless (box-cas! b old new)
        (loop (unbox b))))))

(define async-atomic-box-ref
  (lambda (b)
    (let loop ([value (unbox b)])
      (if (box-cas! b value value)
          value
          (loop (unbox b))))))

(define async-atomic-box-add!
  (lambda (b delta)
    (let loop ([old (async-atomic-box-ref b)])
      (let ([new (+ old delta)])
        (if (box-cas! b old new)
            new
            (loop (async-atomic-box-ref b)))))))

(define async-work-deque-grow!
  (lambda (deque top bottom old-ring)
    (let* ([old-slots (async-work-ring-slots old-ring)]
           [old-mask (async-work-ring-mask old-ring)]
           [new-slots (make-vector (fx* 2 (vector-length old-slots)) #f)]
           [new-mask (fx- (vector-length new-slots) 1)])
      (let loop ([i top])
        (when (fx< i bottom)
          (vector-set! new-slots (fxand i new-mask)
            (vector-ref old-slots (fxand i old-mask)))
          (loop (fx+ i 1))))
      (let ([ring (make-async-work-ring new-slots new-mask)])
        (async-atomic-box-set! (async-work-deque-ring deque) ring)
        ring))))

;;; Monotonic time in microseconds.
(define async-monotonic-us
  (lambda ()
    (let ([t (current-time 'time-monotonic)])
      (+ (* (time-second t) 1000000) (quotient (time-nanosecond t) 1000)))))

(define async-seconds->us
  (lambda (s)
    (inexact->exact (ceiling (* s 1000000)))))

(define async-valid-seconds?
  (lambda (x)
    (and (real? x)
         (not (negative? x))
         (not (eqv? x +inf.0))
         (not (eqv? x +nan.0)))))

;;; -------------------------------------------------- condition objects

(define-condition-type &async-cancellation &condition
  $make-async-cancellation-condition $async-cancellation-condition?
  (reason $async-cancellation-reason))

(define-condition-type &async-channel-closed &condition
  $make-channel-closed-condition $channel-closed-condition?
  (reason $channel-closed-reason))


;;; -------------------------------------------- scheduler-owned storage
;;;
;;; The current scheduler is native-thread-local worker state.  Async task
;;; switches never capture or install Chez thread parameters.

(define $async-scheduler
  ($make-thread-parameter #f (lambda (x) x)))

;;; Initialized after the public task procedures are assembled.  The internal
;;; entry accepts non-inherited termination actions for scoped task creation.
(define async-spawn-task #f)

;;; -------------------------------------------------------- sync states
;;;
;;; A sync state owns an atomic state box holding one of:
;;;   'waiting                 no claim yet
;;;   'claimed                 transient: a completer owns it
;;;   (done . payload)         final; payload = (values . vals) | (raise . c)
;;;
;;; Completion, cancellation, and failure compete with box-cas!.

(define-record-type (async-sync-state make-async-sync-state% async-sync-state?)
  (nongenerative async-sync-state-layer7)
  (sealed #t)
  (fields
    (immutable state)               ; atomic box: waiting | claimed | (done . payload)
    (immutable mutex-box)           ; lazily allocated registration mutex
    (mutable registration-phase)    ; new | registering | registered
    (mutable cancel-pending?)
    (mutable nack)
    (mutable slots)))               ; lazily allocated token -> per-perform state

(define make-async-sync-state
  (lambda ()
    (make-async-sync-state% (box 'waiting) (box #f)
      'new #f #f #f)))

(define async-sync-state-mutex
  (lambda (ss)
    (let ([mutex-box (async-sync-state-mutex-box ss)])
      (or (async-atomic-box-ref mutex-box)
          (let ([mutex (make-async-os-mutex)])
            (if (box-cas! mutex-box #f mutex)
                mutex
                (async-atomic-box-ref mutex-box)))))))

(define async-sync-state-live?
  (lambda (ss) (eq? (unbox (async-sync-state-state ss)) 'waiting)))

(define async-sync-state-claim!
  (lambda (ss)
    (box-cas! (async-sync-state-state ss) 'waiting 'claimed)))

(define async-sync-state-complete!
  (lambda (ss payload)
    (set-box! (async-sync-state-state ss) (cons 'done payload))))

;;; Registration is a handshake with cancellation.  A cancellation arriving
;;; before or during block publication is deferred until block has returned,
;;; so a waiter cannot be published after its nack has already run.
(define async-sync-begin-registration!
  (lambda (ss nack)
    (with-async-mutex (async-sync-state-mutex ss)
      (async-sync-state-nack-set! ss nack)
      (async-sync-state-registration-phase-set! ss 'registering))))

(define async-sync-end-registration!
  (lambda (ss)
    (with-async-mutex (async-sync-state-mutex ss)
      (async-sync-state-registration-phase-set! ss 'registered)
      (async-sync-state-cancel-pending? ss))))

(define async-sync-request-cancel!
  (lambda (ss)
    (with-async-mutex (async-sync-state-mutex ss)
      (case (async-sync-state-registration-phase ss)
        [(new registering)
         (async-sync-state-cancel-pending?-set! ss #t)
         (values #f #f)]
        [else
         (if (async-sync-state-claim! ss)
             (values #t (async-sync-state-nack ss))
             (values #f #f))]))))

;;; Per-operation slots make an immutable operation safe to perform more than
;;; once concurrently.  The synchronization state identifies one perform.
(define async-sync-slot-set!
  (lambda (ss token value)
    (with-async-mutex (async-sync-state-mutex ss)
      (let ([slots (or (async-sync-state-slots ss)
                       (let ([slots (make-eq-hashtable)])
                         (async-sync-state-slots-set! ss slots)
                         slots))])
        (hashtable-set! slots token value)))))

(define async-sync-slot-ref
  (lambda (ss token default)
    (with-async-mutex (async-sync-state-mutex ss)
      (let ([slots (async-sync-state-slots ss)])
        (if slots (hashtable-ref slots token default) default)))))

(define async-sync-slot-delete!
  (lambda (ss token)
    (with-async-mutex (async-sync-state-mutex ss)
      (let ([slots (async-sync-state-slots ss)])
        (when slots (hashtable-delete! slots token))))))

;;; ------------------------------------------------------------ records

(define-record-type (async-scheduler make-async-scheduler% $async-scheduler?)
  (nongenerative async-scheduler-layer14)
  (sealed #t)
  (fields
    (immutable prompt-tag)          ; shared one-shot suspension prompt
    (mutable preemption-exit)       ; escape from the active preemptive turn
    (immutable group $async-scheduler-group) ; owning scheduler group
    (immutable group-index)         ; stable index within the group
    (immutable current-queue)       ; tasks run this turn
    (immutable next-queue)          ; tasks run next turn
    (immutable work-deque)          ; owner-bottom/thief-top Chase--Lev deque
    (immutable remote-queue)        ; cross-thread submissions
    (immutable remote-mutex)
    (immutable remote-cond)
    (mutable tasks)                 ; eq-hashtable id -> task
    (mutable task-count $async-scheduler-task-count $async-scheduler-task-count-set!)
    (mutable next-id)
    (mutable owner-thread)          ; thread id running the loop, or #f
    (mutable status)                ; created | running | shutdown
    (mutable virtual?)              ; deterministic virtual clock
    (mutable vtime)                 ; virtual clock, microseconds
    (immutable timers)              ; indexed min-heap of async-timer
    (mutable current-task)          ; running task or #f
    (mutable in-switch?)            ; #t while unwinding/rewinding a switch
    (mutable saved-exception-state) ; ambient exception state
    (mutable turn-count $async-scheduler-turn-count $async-scheduler-turn-count-set!)
    (mutable exec-count)
    (immutable preemption-ticks)   ; #f for cooperative scheduling
    (mutable preemption-count)
    (mutable suspension-count $async-scheduler-suspension-count $async-scheduler-suspension-count-set!)
    (immutable wakeup-count-box)
    (mutable wake-proc)             ; io layer wakeup hook, or #f
    (mutable poll-proc)             ; io layer poll hook: scheduler x block? -> void
    (mutable io-state)))            ; io layer data

(define-record-type (async-scheduler-group make-async-scheduler-group async-scheduler-group?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable prompt-tag)
    (immutable mutex)
    (immutable condition)
    (immutable ready-queue)         ; ready migratable tasks
    (immutable tasks)               ; stable id -> task registry for the group
    (mutable task-count)
    (mutable schedulers)
    (mutable root-task)
    (mutable next-task-id)
    (mutable idle-schedulers)       ; schedulers that may block waiting for work
    (mutable shutdown?)
    (mutable failure)
    (mutable workers)))

(define-record-type (async-timer make-async-timer async-timer?)
  (nongenerative async-timer-layer2)
  (sealed #t)
  (fields
    (immutable deadline)
    (immutable sequence)
    (immutable deliver-box)
    (mutable heap async-timer-owner-heap async-timer-owner-heap-set!)
    (mutable index)))

(define-record-type (async-timer-heap make-async-timer-heap% async-timer-heap?)
  (nongenerative)
  (sealed #t)
  (fields
    (mutable items)
    (mutable size)
    (mutable next-sequence)
    (immutable mutex)))

(define make-async-timer-heap
  (lambda ()
    (make-async-timer-heap% (make-vector 16 #f) 0 0 (make-async-os-mutex))))

(define-record-type (async-context make-async-context% $async-context?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable parent)
    (immutable mutex)
    (immutable canceled-box async-context-canceled-box)
    (mutable reason async-context-reason/raw async-context-reason/raw-set!)
    (mutable children)               ; context -> context
    (mutable waiters)                ; sync state -> deliver
    (mutable deadline-cancel)))

(define-record-type (async-task make-async-task $async-task?)
  (nongenerative async-task-layer13)
  (sealed #t)
  (fields
    (immutable id)
    (immutable name)
    (mutable state)                 ; ready running waiting completed failed canceled
    (mutable entry)                 ; thunk before first run, #f after
    (mutable resumption)            ; one-shot procedure or raw timer capture
    (mutable preempted?)             ; resume first, then rearm the task timer
    (immutable scheduler-group)     ; scheduler group in which the task runs
    (mutable scheduler)             ; current or most recent execution scheduler
    (mutable wait-scheduler)        ; scheduler that owns the current wait
    (immutable migratable?)
    (mutable resume-pinned?)        ; next I/O resumption stays on wait owner
    (mutable suspension-state)      ; #f | unwinding | parking | parked | delivered
    ;; Chez exception-handler state is VM dynamic state.  It is activated
    ;; separately at scheduler boundaries.
    (mutable exception-state async-task-exception-state
                             async-task-exception-state-set!)
    (mutable parent-group)          ; group this task belongs to
    (immutable context)             ; task-owned cancellation context
    (mutable context-override)      ; dynamically scoped ambient context or #f
    (mutable child-group)           ; group owned by this task, or #f
    (mutable result-values)
    (mutable failure-condition)     ; failure or cancellation condition
    (mutable join-waiters)          ; list of (ss . deliver)
    (mutable observed?)             ; failure observed by a joiner
    (immutable cancel-state-box)    ; atomic box: #f | requested
    (mutable cancel-condition)
    (mutable current-wait)          ; wait description or #f
    (mutable nack-thunk)            ; withdraws the current wait
    (mutable payload)               ; pending delivery for a ready task
    (mutable sync-state)            ; box of the current wait, or #f
    (immutable cancel-shield-box)   ; atomic box: cancellation temporarily masked
    (mutable termination-actions)   ; trusted hooks run exactly at termination
    (mutable owned-mutexes)         ; async locks released at termination
    (immutable mutex)))

(define-record-type (async-task-group make-async-task-group% $async-task-group?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable children)            ; eq-hashtable of member tasks
    (mutable child-count)
    (mutable subgroups)             ; linked descendant groups
    (mutable waiters)               ; list of (ss . deliver)
    (mutable unobserved)            ; unobserved child failure conditions
    (mutable parent)                ; parent group or #f
    (immutable context)
    (immutable mutex)))

;;; Cancellation is published across scheduler workers.  Atomic reads pair
;;; the flag with condition/reason initialization performed before the flag is
;;; set, while shield transitions remain visible to foreign completers.
(define async-context-canceled?/raw
  (lambda (context)
    (and (async-atomic-box-ref (async-context-canceled-box context)) #t)))

(define async-context-canceled?/raw-set!
  (lambda (context value)
    (async-atomic-box-set! (async-context-canceled-box context) value)))

(define async-task-cancel-state
  (lambda (task)
    (async-atomic-box-ref (async-task-cancel-state-box task))))

(define async-task-cancel-state-set!
  (lambda (task value)
    (async-atomic-box-set! (async-task-cancel-state-box task) value)))

(define async-task-cancel-shield?
  (lambda (task)
    (and (async-atomic-box-ref (async-task-cancel-shield-box task)) #t)))

(define async-task-cancel-shield?-set!
  (lambda (task value)
    (async-atomic-box-set! (async-task-cancel-shield-box task) value)))

(define-record-type (operation make-async-operation $async-operation?)
  (nongenerative)
  (sealed #t)
  (fields (immutable try) (immutable block) (immutable wrap) (immutable nack)))

(define-record-type (async-choice-result make-async-choice-result async-choice-result?)
  (nongenerative)
  (sealed #t)
  (fields (immutable operation) (immutable values)))

(define-record-type (async-future make-async-future% $async-future?)
  (nongenerative)
  (sealed #t)
  (fields (mutable state)           ; box: waiting | claimed | (done . payload)
          (mutable waiters)         ; list of (ss . deliver)
          (immutable mutex)))

(define-record-type (async-channel make-async-channel% $async-channel?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable capacity)            ; 0 = unbuffered rendezvous
    (immutable mutex)
    (mutable buffer)                ; ring vector when capacity > 0
    (mutable bstart)
    (mutable bcount)
    (mutable puts)                  ; list of (value ss . deliver)
    (mutable gets)                  ; list of (ss . deliver)
    (mutable closed?)
    (mutable close-reason)))

(define-record-type (async-fiber-mutex make-async-fiber-mutex% $async-mutex?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable mutex)
    (mutable owner)                 ; task or #f
    (mutable waiters)))             ; FIFO list of (task ss . deliver)

(define-record-type (async-rw-mutex make-async-rw-mutex% $async-rw-mutex?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable mutex)
    (mutable writer)                ; task or #f
    (immutable readers)             ; task -> recursive read count
    (mutable waiters)))             ; FIFO vectors: mode task ss deliver

(define-record-type (async-wait-group make-async-wait-group% $async-wait-group?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable mutex)
    (mutable count)
    (mutable waiters)))             ; list of (ss . deliver)

(define-record-type (async-once make-async-once% $async-once?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable mutex)
    (mutable state)                 ; pending | running | done
    (mutable owner)                 ; task while running
    (mutable waiters)))             ; list of (ss . deliver)

(define-record-type (async-condition make-async-condition% $async-condition?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable mutex async-condition-guard)
    (immutable lock)                ; async mutex or async rw mutex
    (mutable waiters)))             ; FIFO list of (ss . deliver)






(define async-current-task/required
  (lambda (who)
    (let ([sched ($async-scheduler)])
      (unless (and ($async-scheduler? sched)
                   (async-scheduler-current-task sched))
        ($oops who "called outside of an async task"))
      (async-scheduler-current-task sched))))

;;; ------------------------------------------------ cancellation contexts

(define async-current-context
  (lambda ()
    (let ([sched ($async-scheduler)])
      (and ($async-scheduler? sched)
           (let ([task (async-scheduler-current-task sched)])
             (and task
                  (or (async-task-context-override task)
                      (async-task-context task))))))))

(define async-context-canceled?/locked
  (lambda (context)
    (async-context-canceled?/raw context)))

(define async-cancel-timer!
  (lambda (timer)
    (when timer
      (let* ([deliver-box (async-timer-deliver-box timer)]
             [deliver (unbox deliver-box)])
        (when (and deliver (box-cas! deliver-box deliver #f))
          (let ([heap (async-timer-owner-heap timer)])
            (when heap
              (with-async-mutex (async-timer-heap-mutex heap)
                (async-timer-heap-remove/raw! heap timer)))))))))

(define async-context-detach!
  (lambda (context)
    (let ([parent (async-context-parent context)])
      (when parent
        (with-async-mutex (async-context-mutex parent)
          (hashtable-delete! (async-context-children parent) context))))))

(define async-context-cancel-core!
  (lambda (context reason)
    (let-values ([(won? children waiters cancel-timer)
                  (with-async-mutex (async-context-mutex context)
                    (if (async-context-canceled?/locked context)
                        (values #f '() '() #f)
                        (let ([children
                               (hashtable-values
                                 (async-context-children context))]
                              [waiters
                               (hashtable-values
                                 (async-context-waiters context))]
                              [cancel-timer
                               (async-context-deadline-cancel context)])
                          (async-context-reason/raw-set! context reason)
                          (hashtable-clear! (async-context-children context))
                          (hashtable-clear! (async-context-waiters context))
                          (async-context-deadline-cancel-set! context #f)
                          (async-context-canceled?/raw-set! context #t)
                          (values #t children waiters cancel-timer))))])
      (when won?
        (async-context-detach! context)
        (when cancel-timer (cancel-timer))
        (vector-for-each
          (lambda (deliver) (deliver (cons 'values '())))
          waiters)
        (vector-for-each
          (lambda (child) (async-context-cancel-core! child reason))
          children))
      won?)))

(define async-make-context
  (lambda (parent)
    (let ([context
           (make-async-context% parent (make-async-os-mutex)
             (box #f) #f (make-eq-hashtable) (make-eq-hashtable) #f)])
      (when parent
        (let-values ([(canceled? reason)
                      (with-async-mutex (async-context-mutex parent)
                        (if (async-context-canceled?/locked parent)
                            (values #t (async-context-reason/raw parent))
                            (begin
                              (hashtable-set!
                                (async-context-children parent)
                                context context)
                              (values #f #f))))])
          (when canceled?
            (async-context-cancel-core! context reason))))
      context)))

(define async-context-operation
  (lambda (context)
    (make-async-operation
      (lambda (ss)
        (with-async-mutex (async-context-mutex context)
          (and (async-context-canceled?/locked context)
               (cons 'values '()))))
      (lambda (ss deliver)
        (let ([ready?
               (with-async-mutex (async-context-mutex context)
                 (if (async-context-canceled?/locked context)
                     #t
                     (begin
                       (hashtable-set! (async-context-waiters context)
                         ss deliver)
                       #f)))])
          (if ready?
              (begin (deliver (cons 'values '())) #f)
              (list 'context context))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-context-mutex context)
          (hashtable-delete! (async-context-waiters context) ss))))))

(define async-context-register-waiter!
  (lambda (context ss deliver)
    (with-async-mutex (async-context-mutex context)
      (cond
        [(not (async-sync-state-live? ss)) #f]
        [(async-context-canceled?/locked context) 'canceled]
        [else
         (hashtable-set! (async-context-waiters context) ss deliver)
         #t]))))

(define async-context-unregister-waiter!
  (lambda (context ss)
    (with-async-mutex (async-context-mutex context)
      (hashtable-delete! (async-context-waiters context) ss))))

(define async-context-cancellation-condition
  (lambda (context)
    (make-async-cancellation-condition (async-context-reason/raw context))))

(define async-install-context-deadline!
  (lambda (who context deadline-us)
    (let ([sched ($async-scheduler)])
      (unless (and ($async-scheduler? sched)
                   (async-scheduler-current-task sched))
        ($oops who "deadline context creation outside of an async task"))
      (let* ([timer
              (async-schedule-timer! sched deadline-us
                (lambda (payload)
                  (async-context-cancel-core! context 'deadline-exceeded)))]
             [cancel (lambda () (async-cancel-timer! timer))]
             [already-canceled?
              (with-async-mutex (async-context-mutex context)
                (if (async-context-canceled?/locked context)
                    #t
                    (begin
                      (async-context-deadline-cancel-set! context cancel)
                      #f)))])
        (when already-canceled? (cancel)))
      context)))

;;; ------------------------------------------- scheduler/task internal helpers




(define task-terminal?
  (lambda (task)
    (memq (async-task-state task) '(completed failed canceled))))

(define task-cancel-requested?
  (lambda (task)
    (or (and (async-task-cancel-state task) #t)
        (async-context-canceled?/raw (async-task-context task)))))

(define task-cancellation-condition
  (lambda (task)
    (or (async-task-cancel-condition task)
        (async-context-cancellation-condition (async-task-context task)))))

;;; Cancellation points raise whenever cancellation is in effect, unless the
;;; current wait is shielded (internal group draining).
(define async-check-cancellation!
  (lambda (task)
    (when (and (task-cancel-requested? task)
               (not (async-task-cancel-shield? task)))
      (raise (task-cancellation-condition task)))))

(define set-sched-switch!
  (lambda (sched v)
    (async-scheduler-in-switch?-set! sched v)))

(define set-current-sched-switch!
  (lambda (v)
    (let ([sched ($async-scheduler)])
      (when (async-scheduler? sched)
        (set-sched-switch! sched v)))))

(define $async-scheduling-switch?
  (lambda ()
    (let ([sched ($async-scheduler)])
      (and (async-scheduler? sched)
           (async-scheduler-in-switch? sched)))))

(define async-task-group-runnable?
  (lambda (task)
    (and (async-task-migratable? task)
         (async-group-parallel? (async-task-scheduler-group task)))))

(define sched-now
  (lambda (sched)
    (if (async-scheduler-virtual? sched)
        (async-scheduler-vtime sched)
        (async-monotonic-us))))

(define install-task-exception-state!
  (lambda (sched task)
    (async-scheduler-saved-exception-state-set! sched
      (current-exception-state))
    (current-exception-state (async-task-exception-state task))
    ($async-scheduler sched)))

(define snapshot-task-exception-state!
  (lambda (sched task)
    (async-task-exception-state-set! task (current-exception-state))))

(define restore-scheduler-exception-state!
  (lambda (sched)
    (current-exception-state
      (async-scheduler-saved-exception-state sched))
    ($async-scheduler sched)))

(define sched-registry-add/raw!
  (lambda (sched task)
    (let* ([group ($async-scheduler-group sched)]
           [tasks (async-scheduler-group-tasks group)]
           [id (async-task-id task)])
      (unless (hashtable-ref tasks id #f)
        (hashtable-set! tasks id task)
        (async-scheduler-group-task-count-set! group
          (fx+ 1 (async-scheduler-group-task-count group))))
      (async-invariant
        (fx= (async-scheduler-group-task-count group)
             (hashtable-size tasks))
        "task registry count does not match registry size" group))))

(define sched-registry-remove/raw!
  (lambda (sched task)
    (let* ([group ($async-scheduler-group sched)]
           [tasks (async-scheduler-group-tasks group)]
           [id (async-task-id task)])
      (when (hashtable-ref tasks id #f)
        (hashtable-delete! tasks id)
        (async-scheduler-group-task-count-set! group
          (fx- (async-scheduler-group-task-count group) 1)))
      (async-invariant
        (fx= (async-scheduler-group-task-count group)
             (hashtable-size tasks))
        "task registry count does not match registry size" group))))

(define sched-registry-add!
  (lambda (sched task)
    (with-async-mutex
      (async-scheduler-group-mutex ($async-scheduler-group sched))
      (sched-registry-add/raw! sched task))))

(define sched-registry-remove!
  (lambda (sched task)
    (with-async-mutex
      (async-scheduler-group-mutex ($async-scheduler-group sched))
      (sched-registry-remove/raw! sched task))))

(define async-group-parallel?
  (lambda (group)
    (fx> (vector-length (async-scheduler-group-schedulers group)) 1)))

(define async-group-has-work?
  (lambda (group)
    (let ([schedulers (async-scheduler-group-schedulers group)])
      (let loop ([i 0])
        (and (fx< i (vector-length schedulers))
             (let* ([deque
                     (async-scheduler-work-deque (vector-ref schedulers i))]
                    [top (async-atomic-box-ref (async-work-deque-top deque))]
                    [bottom
                     (async-atomic-box-ref (async-work-deque-bottom deque))])
               (or (fx< top bottom) (loop (fx+ i 1)))))))))

(define async-debug-check-owner!
  (lambda (sched)
    (when async-debug-invariants?
      (async-invariant (eq? ($async-scheduler) sched)
        "scheduler is running with a different current scheduler" sched)
      (async-invariant (eq? (async-scheduler-status sched) 'running)
        "scheduler owner operation ran outside the running state" sched)
      (async-invariant
        (and (async-scheduler-owner-thread sched)
             (fx= (async-scheduler-owner-thread sched) (get-thread-id)))
        "scheduler owner operation ran on a foreign thread" sched))))

(define async-debug-check-group-quiescent!
  (lambda (group)
    (when async-debug-invariants?
      (with-async-mutex (async-scheduler-group-mutex group)
        (async-invariant
          (and (fx= (async-scheduler-group-task-count group) 0)
               (fx= (hashtable-size (async-scheduler-group-tasks group)) 0))
          "scheduler group retained terminal tasks" group)
        (async-invariant (not (async-group-has-work? group))
          "scheduler group retained stealable work" group))
      (with-async-mutex async-debug-runnable-mutex
        (let-values ([(tasks locations)
                      (hashtable-entries async-debug-runnable)])
          (vector-for-each
            (lambda (task)
              (async-invariant
                (not (eq? (async-task-scheduler-group task) group))
                "scheduler group retained a runnable task" task))
            tasks))))))

(define async-group-take-idle/raw!
  (lambda (group)
    (let ([idle (async-scheduler-group-idle-schedulers group)])
      (if (null? idle)
          #f
          (begin
            (async-scheduler-group-idle-schedulers-set! group (cdr idle))
            (car idle))))))

(define async-group-next-wake-target!
  (lambda (group)
    (with-async-mutex (async-scheduler-group-mutex group)
      (async-group-take-idle/raw! group))))

(define async-group-mark-idle!
  (lambda (sched)
    (let ([group ($async-scheduler-group sched)])
      (with-async-mutex (async-scheduler-group-mutex group)
        (unless (memq sched (async-scheduler-group-idle-schedulers group))
          (async-scheduler-group-idle-schedulers-set! group
            (cons sched (async-scheduler-group-idle-schedulers group))))))))

(define async-group-unmark-idle!
  (lambda (sched)
    (let ([group ($async-scheduler-group sched)])
      (with-async-mutex (async-scheduler-group-mutex group)
        (async-scheduler-group-idle-schedulers-set! group
          (remq sched (async-scheduler-group-idle-schedulers group)))))))

(define async-work-push!
  (lambda (sched task)
    (async-debug-check-owner! sched)
    (let* ([deque (async-scheduler-work-deque sched)]
           [bottom (async-atomic-box-ref (async-work-deque-bottom deque))]
           [top (async-atomic-box-ref (async-work-deque-top deque))]
           [ring0 (async-atomic-box-ref (async-work-deque-ring deque))]
           [ring
            (if (fx>= (fx- bottom top) (async-work-ring-mask ring0))
                (async-work-deque-grow! deque top bottom ring0)
                ring0)])
      (async-debug-queue-claim! task 'work)
      (vector-set! (async-work-ring-slots ring)
        (fxand bottom (async-work-ring-mask ring)) task)
      ;; Publish only after the ring slot is visible to thieves.
      (async-atomic-box-set! (async-work-deque-bottom deque) (fx+ bottom 1)))))

(define async-work-pop!
  (lambda (sched)
    (async-debug-check-owner! sched)
    (let* ([deque (async-scheduler-work-deque sched)]
           [bottom0 (async-atomic-box-ref (async-work-deque-bottom deque))]
           [bottom (fx- bottom0 1)])
      (async-atomic-box-set! (async-work-deque-bottom deque) bottom)
      (let* ([top (async-atomic-box-ref (async-work-deque-top deque))]
             [task
              (cond
                [(fx< bottom top)
                 (async-atomic-box-set! (async-work-deque-bottom deque) top)
                 #f]
                [else
                 (let* ([ring (async-atomic-box-ref (async-work-deque-ring deque))]
                        [slot (fxand bottom (async-work-ring-mask ring))]
                        [task (vector-ref (async-work-ring-slots ring) slot)])
                   (if (and (fx= top bottom)
                            (not (box-cas! (async-work-deque-top deque)
                                          top (fx+ top 1))))
                       (begin
                         (async-atomic-box-set!
                           (async-work-deque-bottom deque) (fx+ top 1))
                         #f)
                       (begin
                         (vector-set! (async-work-ring-slots ring) slot #f)
                         (when (fx= top bottom)
                           (async-atomic-box-set!
                             (async-work-deque-bottom deque) (fx+ top 1)))
                         (async-debug-queue-release! task)
                         task)))])])
        task))))

(define async-work-steal!
  (lambda (victim)
    (let* ([deque (async-scheduler-work-deque victim)]
           [top (async-atomic-box-ref (async-work-deque-top deque))]
           [bottom (async-atomic-box-ref (async-work-deque-bottom deque))]
           [task
            (and (fx< top bottom)
                 (let* ([ring (async-atomic-box-ref (async-work-deque-ring deque))]
                        [slot (fxand top (async-work-ring-mask ring))]
                        [task (vector-ref (async-work-ring-slots ring) slot)])
                   (and (box-cas! (async-work-deque-top deque) top (fx+ top 1))
                        (begin
                          (vector-set! (async-work-ring-slots ring) slot #f)
                          (async-debug-queue-release! task)
                          task))))])
      task)))

(define async-adopt-work!
  (lambda (sched task)
    (unless (eq? (async-task-scheduler task) sched)
      (async-task-scheduler-set! task sched))
    task))

(define async-take-work!
  (lambda (sched)
    (let ([local (async-work-pop! sched)])
      (if local
          (async-adopt-work! sched local)
          (let ([injected (async-group-take-ready! sched)])
            (if injected
                injected
                (let* ([group ($async-scheduler-group sched)]
                       [schedulers (async-scheduler-group-schedulers group)]
                       [n (vector-length schedulers)]
                       [start (async-scheduler-group-index sched)])
                  (let loop ([offset 1])
                    (if (fx= offset n)
                        #f
                        (let ([task
                               (async-work-steal!
                                 (vector-ref schedulers
                                   (fxmod (fx+ start offset) n)))])
                          (if task
                              (async-adopt-work! sched task)
                              (loop (fx+ offset 1)))))))))))))

(define async-work-submit!
  (lambda (task preferred)
    (if (and (eq? ($async-scheduler) preferred)
             (async-scheduler-owner-thread preferred)
             (fx= (async-scheduler-owner-thread preferred) (get-thread-id)))
        (begin
          (async-work-push! preferred task)
          (let ([target
                 (async-group-next-wake-target!
                   (async-task-scheduler-group task))])
            (when target (async-wake-scheduler target))))
        (async-group-submit! task))))

(define async-group-wake!
  (lambda (group)
    (if-feature pthreads
      (with-mutex (async-scheduler-group-mutex group)
        (condition-broadcast (async-scheduler-group-condition group)))
      (void))
    (vector-for-each
      (lambda (sched)
        (let ([wake (async-scheduler-wake-proc sched)])
          (when wake (wake)))
        (if-feature pthreads
          (with-mutex (async-scheduler-remote-mutex sched)
            (condition-broadcast (async-scheduler-remote-cond sched)))
          (void)))
      (async-scheduler-group-schedulers group))))

(define async-group-submit!
  (lambda (task)
    (let ([group (async-task-scheduler-group task)])
      (let ([target
             (with-async-mutex (async-scheduler-group-mutex group)
               (async-debug-queue-claim! task 'group-ready)
               (async-queue-push! (async-scheduler-group-ready-queue group) task)
               (async-group-take-idle/raw! group))])
        (when target (async-wake-scheduler target))))))

(define async-group-take-ready!
  (lambda (sched)
    (let ([group ($async-scheduler-group sched)])
      (with-async-mutex (async-scheduler-group-mutex group)
        (let loop ()
          (let ([q (async-scheduler-group-ready-queue group)])
            (cond
              [(async-queue-empty? q) #f]
              [else
               (let ([task (async-queue-pop! q)])
                 (async-debug-queue-release! task)
                 (if (and (eq? (async-task-state task) 'ready)
                          (async-task-group-runnable? task))
                     (let ([old (async-task-scheduler task)])
                       (unless (eq? old sched)
                         (async-task-scheduler-set! task sched))
                       task)
                     (loop)))])))))))

(define async-group-shutdown!
  (lambda (group)
    (with-async-mutex (async-scheduler-group-mutex group)
      (async-scheduler-group-shutdown?-set! group #t)
      (if-feature pthreads
        (condition-broadcast (async-scheduler-group-condition group))
        (void)))
    (async-group-wake! group)))

(define async-publish-ready!
  (lambda (task completion-sched)
    (if (and (async-task-group-runnable? task)
             (not (async-task-resume-pinned? task))
             ;; Raising through a captured suspension is entered on the
             ;; scheduler that owns the wait.  Normal values remain free to
             ;; migrate before resumption.
             (let ([payload (async-task-payload task)])
               (not (and (pair? payload) (eq? (car payload) 'raise)))))
        (async-work-submit! task completion-sched)
        (if (and (async-scheduler-owner-thread completion-sched)
                 (fx= (async-scheduler-owner-thread completion-sched)
                      (get-thread-id)))
            (begin
              (async-debug-queue-claim! task 'next)
              (async-queue-push!
                (async-scheduler-next-queue completion-sched) task))
            (async-remote-submit completion-sched task)))))

;;; Deliver a payload to a suspended task.  May run on a foreign thread.  A
;;; completion can race with the old scheduler unwinding the suspension.  In
;;; that case, record the delivery until the old execution has fully restored
;;; its scheduler state.
(define $async-deliver-task
  (lambda (task payload)
    (let-values ([(completion-sched publish?)
                  (with-async-mutex (async-task-mutex task)
                    (let ([completion-sched
                           (or (async-task-wait-scheduler task)
                               (async-task-scheduler task))])
                      (async-task-payload-set! task payload)
                      (async-task-current-wait-set! task #f)
                      (async-task-nack-thunk-set! task #f)
                      (if (eq? (async-task-suspension-state task) 'parked)
                          (begin
                            (async-task-suspension-state-set! task #f)
                            (async-task-state-set! task 'ready)
                            (values completion-sched #t))
                          (begin
                            (async-task-suspension-state-set! task 'delivered)
                            (values completion-sched #f)))))])
      (async-atomic-box-add! (async-scheduler-wakeup-count-box completion-sched) 1)
      (when publish?
        (async-publish-ready! task completion-sched)))))

;;; Mark the suspension as leaving the task runner.  Publication is deferred
;;; until async-run-task-once has returned to the scheduler loop, so the reset
;;; call that captured the continuation is no longer active.
(define async-finish-suspension!
  (lambda (task)
    (with-async-mutex (async-task-mutex task)
      (unless (eq? (async-task-suspension-state task) 'delivered)
        (async-task-suspension-state-set! task 'parking)))))

;;; Complete the handoff after the task runner has returned.  A result that
;;; arrived during unwind is published here; otherwise later delivery can
;;; publish directly from the parked state.
(define async-complete-suspension!
  (lambda (task)
    (let-values ([(completion-sched publish?)
                  (with-async-mutex (async-task-mutex task)
                    (if (eq? (async-task-suspension-state task) 'delivered)
                        (begin
                          (async-task-suspension-state-set! task #f)
                          (async-task-state-set! task 'ready)
                          (values
                            (or (async-task-wait-scheduler task)
                                (async-task-scheduler task))
                            #t))
                        (begin
                          (async-task-suspension-state-set! task 'parked)
                          (values #f #f))))])
      (when publish?
        (async-publish-ready! task completion-sched)))))

(define async-remote-submit
  (lambda (sched task)
    (with-async-mutex (async-scheduler-remote-mutex sched)
      (async-debug-queue-claim! task 'remote)
      (async-queue-push! (async-scheduler-remote-queue sched) task))
    (async-wake-scheduler sched)))

(define async-wake-scheduler
  (lambda (sched)
    (let ([wake (async-scheduler-wake-proc sched)])
      (when wake (wake)))
    (if-feature pthreads
      (with-mutex (async-scheduler-remote-mutex sched)
        (condition-signal (async-scheduler-remote-cond sched)))
      (void))))

;;; The single claim point for completing a suspended task.  Returns #t when
;;; the claim succeeded.
(define-record-type (async-delivery-reservation
                      make-async-delivery-reservation
                      async-delivery-reservation?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable sync-state)
    (immutable task)
    (immutable payload)
    (immutable active-box)
    (mutable prepare-actions)))

(define async-delivery-reserve
  (lambda (ss task payload)
    (and (async-sync-state-claim! ss)
         (make-async-delivery-reservation ss task payload (box #t) '()))))

(define async-delivery-add-action!
  (lambda (reservation action)
    (async-delivery-reservation-prepare-actions-set! reservation
      (cons action (async-delivery-reservation-prepare-actions reservation)))
    reservation))

(define async-delivery-prepare!
  (lambda (reservation)
    (and (box-cas! (async-delivery-reservation-active-box reservation) #t #f)
         (begin
           (async-sync-state-complete!
             (async-delivery-reservation-sync-state reservation)
             (async-delivery-reservation-payload reservation))
           (lambda ()
             (for-each
               (lambda (action) (action))
               (reverse
                 (async-delivery-reservation-prepare-actions reservation)))
             ($async-deliver-task
               (async-delivery-reservation-task reservation)
               (async-delivery-reservation-payload reservation)))))))

(define async-delivery-rollback!
  (lambda (reservation)
    (and (box-cas! (async-delivery-reservation-active-box reservation) #t #f)
         (box-cas!
           (async-sync-state-state
             (async-delivery-reservation-sync-state reservation))
           'claimed 'waiting))))

(define async-delivery-prepare-all!
  (lambda (reservations)
    (map async-delivery-prepare! reservations)))

(define async-delivery-publish-all!
  (lambda (publications)
    (for-each (lambda (publish) (when publish (publish))) publications)))

(define $async-make-deliver
  (lambda (ss task)
    (case-lambda
      [(payload)
       (let ([reservation (async-delivery-reserve ss task payload)])
         (and reservation
              (let ([publish (async-delivery-prepare! reservation)])
                (publish)
                #t)))]
      [(command payload)
       (case command
         [(reserve) (async-delivery-reserve ss task payload)]
         [else ($oops 'async-deliver "invalid delivery command ~s" command)])])))

;;; Cancellation of a waiting task: claim, nack, resume with condition.
(define $async-cancel-waiting-task
  (lambda (task)
    (let ([ss (async-task-sync-state task)])
      (when ss
        (let-values ([(claimed? nack) (async-sync-request-cancel! ss)])
          (when claimed?
            (when nack (nack))
            (let ([payload (cons 'raise (task-cancellation-condition task))])
              (async-sync-state-complete! ss payload)
              ($async-deliver-task task payload))))))))

;;; ------------------------------------------------------------- task groups

(define make-async-group
  (lambda (parent context)
    (make-async-task-group% (make-eq-hashtable) 0 '()
      (make-async-wait-queue) '() parent context
      (make-async-os-mutex))))

(define group-cancel-children!
  (lambda (grp reason)
    (async-context-cancel-core! (async-task-group-context grp) reason)
    (let-values ([(children subgroups)
                  (with-async-mutex (async-task-group-mutex grp)
                    (values
                      (hashtable-values (async-task-group-children grp))
                      (async-task-group-subgroups grp)))])
      (vector-for-each
        (lambda (t) (task-cancel! t reason))
        children)
      (for-each
        (lambda (g) (group-cancel-children! g reason))
        subgroups))))

(define group-child-terminated!
  (lambda (grp task)
    (let ([waiters
           (with-async-mutex (async-task-group-mutex grp)
             (when (hashtable-ref (async-task-group-children grp) task #f)
               (hashtable-delete! (async-task-group-children grp) task)
               (async-task-group-child-count-set! grp
                 (fx- (async-task-group-child-count grp) 1)))
             (when (eq? (async-task-state task) 'failed)
               (async-task-group-unobserved-set! grp
                 (cons (cons task (async-task-failure-condition task))
                       (async-task-group-unobserved grp))))
             (if (fx= (async-task-group-child-count grp) 0)
                 (async-wait-queue-drain! (async-task-group-waiters grp))
                 '()))])
      (for-each (lambda (w) ((cdr w) (cons 'values '()))) waiters))))

(define group-empty-operation
  (lambda (grp)
    (let ([token (list 'task-group-empty-operation)])
      (make-async-operation
      (lambda (ss)
        (with-async-mutex (async-task-group-mutex grp)
          (and (fx= (async-task-group-child-count grp) 0)
               (cons 'values '()))))
      (lambda (ss deliver)
        (let ([blocked?
               (with-async-mutex (async-task-group-mutex grp)
                 (if (fx= (async-task-group-child-count grp) 0)
                     #f
                     (begin
                       (async-sync-slot-set! ss token
                         (async-wait-queue-add!
                           (async-task-group-waiters grp)
                           (cons ss deliver)))
                       #t)))])
          (if blocked?
              (list 'task-group)
              (begin (deliver (cons 'values '())) #f))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-task-group-mutex grp)
          (let ([node (async-sync-slot-ref ss token #f)])
            (when node
              (async-wait-queue-remove! (async-task-group-waiters grp) node)
              (async-sync-slot-delete! ss token)))))))))



;;; -------------------------------------------------------------- suspension

(define async-suspend-token (list 'async-suspended))

;;; Capture the active task at the scheduler group's private prompt.  The raw
;;; one-shot continuation and its prompt boundary are later spliced directly
;;; onto the worker that claims the task.
(define async-shift1-to-scheduler
  (lambda (sched proc)
    ($control-shift-native1-at
      (async-scheduler-prompt-tag sched) #t
      (lambda (resumption)
        (let ([exit (async-scheduler-preemption-exit sched)])
          (async-invariant exit
            "preemption capture has no active scheduler escape" sched)
          (exit (proc resumption)))))))

;;; One checked suspension operation: capture through the scheduler prompt,
;;; transition running->waiting, publish the wait, return to the scheduler.
(define $async-suspend
  (lambda (sched task ss register!)
    (async-check-cancellation! task)
    (snapshot-task-exception-state! sched task)
    (set-sched-switch! sched #t)
    (let ([payload
            ($control-shift1-at
              (async-scheduler-prompt-tag sched) #t
              (lambda (resumption)
                (set-sched-switch! sched #f)
                (with-async-mutex (async-task-mutex task)
                  (async-task-state-set! task 'waiting)
                  (async-task-resumption-set! task resumption)
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
                async-suspend-token))])
      ;; Resolve through the scheduler installed for this execution instead
      ;; of retaining the scheduler that captured the continuation.
      (set-current-sched-switch! #f)
      payload)))

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
    (let ([now (sched-now sched)] [heap (async-scheduler-timers sched)])
      (let loop ()
        (let ([timer (async-timer-heap-pop-due! heap now)])
          (when timer
            (let* ([deliver-box (async-timer-deliver-box timer)]
                   [deliver (unbox deliver-box)])
              (when (and deliver (box-cas! deliver-box deliver #f))
                (deliver (cons 'values '()))))
            (loop)))))))



;;; ----------------------------------------------------------------- futures


(define future-complete!
  (lambda (f payload)
    (let ([waiters
           (with-async-mutex (async-future-mutex f)
             (unless (box-cas! (async-future-state f) 'waiting 'claimed)
               ($oops 'future-fulfil! "future is already fulfilled"))
             (let ([waiters
                    (async-wait-queue-drain! (async-future-waiters f))])
               (set-box! (async-future-state f) (cons 'done payload))
               waiters))])
      (for-each (lambda (w) ((cdr w) payload)) waiters))))





;;; ------------------------------------------------------------- async mutexes

(define async-mutex-current-task
  (lambda (who)
    (let ([sched ($async-scheduler)])
      (unless (and ($async-scheduler? sched)
                   (async-scheduler-current-task sched))
        ($oops who "outside of an async task"))
      (async-scheduler-current-task sched))))

(define async-task-add-owned-mutex!
  (lambda (task mutex)
    (with-async-mutex (async-task-mutex task)
      (unless (memq mutex (async-task-owned-mutexes task))
        (async-task-owned-mutexes-set! task
          (cons mutex (async-task-owned-mutexes task)))))))

(define async-task-remove-owned-mutex!
  (lambda (task mutex)
    (with-async-mutex (async-task-mutex task)
      (async-task-owned-mutexes-set! task
        (remq mutex (async-task-owned-mutexes task))))))

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

(define async-fiber-mutex-acquire-operation
  (lambda (mutex)
    (let ([token (list 'async-mutex-acquire-operation)]
          [node-token (list 'async-mutex-acquire-waiter)])
      (make-async-operation
        (lambda (ss)
          (let ([task (async-mutex-current-task
                        'async-mutex-acquire-operation)])
            (async-sync-slot-set! ss token task)
            (let ([result
                   (with-async-mutex (async-fiber-mutex-mutex mutex)
                     (cond
                       [(not (async-fiber-mutex-owner mutex))
                        (async-fiber-mutex-owner-set! mutex task)
                        'acquired]
                       [(eq? (async-fiber-mutex-owner mutex) task)
                        ($oops 'async-mutex-acquire-operation
                          "mutex is not recursive")]
                       [else #f]))])
              (when result (async-task-add-owned-mutex! task mutex))
              (and result (cons 'values '())))))
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
            (async-sync-slot-set! ss token task)
            (with-async-mutex (async-rw-mutex-mutex mutex)
              (cond
                [(eq? mode 'read)
                 (cond
                   [(eq? (async-rw-mutex-writer mutex) task)
                    ($oops 'async-rw-mutex-read-acquire-operation
                      "write owner cannot acquire a read lock")]
                   [(fx> (async-rw-mutex-reader-count mutex task) 0)
                    (async-rw-mutex-add-reader! mutex task)
                    (cons 'values '())]
                   [(and (not (async-rw-mutex-writer mutex))
                         (async-wait-queue-empty?
                           (async-rw-mutex-waiters mutex)))
                    (async-rw-mutex-add-reader! mutex task)
                    (cons 'values '())]
                   [else #f])]
                [else
                 (cond
                   [(eq? (async-rw-mutex-writer mutex) task)
                    ($oops 'async-rw-mutex-acquire-operation
                      "mutex is not recursive")]
                   [(fx> (async-rw-mutex-reader-count mutex task) 0)
                    ($oops 'async-rw-mutex-acquire-operation
                      "read-to-write upgrade is not supported")]
                   [(and (async-rw-mutex-idle? mutex)
                         (async-wait-queue-empty?
                           (async-rw-mutex-waiters mutex)))
                    (async-rw-mutex-writer-set! mutex task)
                    (async-task-add-owned-mutex! task mutex)
                    (cons 'values '())]
                   [else #f])]))))
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





;;; ------------------------------------------------------------------- tasks

(define async-make-task
  (lambda (sched name parent-group parent-context migratable?
           termination-actions entry)
    (let* ([group ($async-scheduler-group sched)]
           [id (with-async-mutex (async-scheduler-group-mutex group)
                 (let ([id (async-scheduler-group-next-task-id group)])
                   (async-scheduler-group-next-task-id-set! group (fx+ 1 id))
                   id))])
      (make-async-task id name 'ready entry #f #f group sched #f migratable? #f #f
        (current-exception-state)
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
        (make-async-os-mutex)))))

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
             (fx= (async-task-group-child-count grp) 0))
           (async-task-cancel-shield?-set! task #f)
           (cond
             [(and (eq? (car outcome) 'done)
                   (task-cancel-requested? task))
              (cons 'failed (task-cancellation-condition task))]
             [(eq? (car outcome) 'done)
              (let find ([us (with-async-mutex (async-task-group-mutex grp)
                               (async-task-group-unobserved grp))])
                (cond
                  [(null? us) outcome]
                  [(async-task-observed? (caar us)) (find (cdr us))]
                  [else (cons 'failed (cdar us))]))]
             [else outcome])]
          [else
           (perform-operation (group-empty-operation grp))
           (loop)])))))

(define terminate-task!
  (lambda (sched task state outcome)
    ;; A task can be canceled after an acquisition has won but before its
    ;; continuation observes the result.  Termination is the final ownership
    ;; backstop for both that race and unscoped acquisitions.
    (let ([owned-locks
           (with-async-mutex (async-task-mutex task)
             (async-task-owned-mutexes task))])
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
             (async-task-resumption-set! task #f)
             (async-task-preempted?-set! task #f)
             (case state
               [(completed) (async-task-result-values-set! task (cdr outcome))]
               [else (async-task-failure-condition-set! task (cdr outcome))])
             (let ([waiters
                    (async-wait-queue-drain!
                      (async-task-join-waiters task))])
               (when (and (eq? state 'failed) (pair? waiters))
                 (async-task-observed?-set! task #t))
               waiters))])
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

(define async-task-join-operation
  (lambda (task)
    (let ([token (list 'task-join-operation)])
      (make-async-operation
      (lambda (ss)
        (let ([sched ($async-scheduler)])
          (when (and (async-scheduler? sched)
                     (eq? task (async-scheduler-current-task sched)))
            ($oops 'task-join-operation "a task cannot join itself")))
        (with-async-mutex (async-task-mutex task)
          (and (task-terminal? task)
               (begin
                 (when (eq? (async-task-state task) 'failed)
                   (async-task-observed?-set! task #t))
                 (task-join-payload task)))))
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
      (async-scheduler-group-prompt-tag group) #f group index
      (make-async-queue) (make-async-queue)
      (make-async-work-deque)
      (make-async-queue) (make-async-os-mutex)
      (if-feature pthreads (make-condition) #f)
      (make-eq-hashtable) 0 0 #f 'created virtual? 0 (make-async-timer-heap) #f #f
      (current-exception-state)
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

(define async-idle-wait
  (lambda (sched)
    (dynamic-wind
      (lambda () (async-group-mark-idle! sched))
      (lambda ()
        (cond
      [(async-scheduler-virtual? sched)
       (let ([timer (async-timer-heap-peek (async-scheduler-timers sched))])
         (if (not timer)
             ($oops 'run-async
               "async deadlock: no runnable tasks and no pending timers")
             (async-scheduler-vtime-set! sched (async-timer-deadline timer))))]
      [(async-scheduler-poll-proc sched)
       => (lambda (poll)
            ;; The preceding nonblocking poll may consume a remote wakeup
            ;; that arrived after the scheduler drained its queues.  Recheck
            ;; under the queue locks before entering a blocking event-loop
            ;; poll.  A submission after this check leaves uv_async pending.
            (let ([group ($async-scheduler-group sched)])
              (if (async-group-parallel? group)
                  (let ([idle?
                         (with-mutex (async-scheduler-group-mutex group)
                           (and
                             (async-queue-empty?
                               (async-scheduler-group-ready-queue group))
                             (not (async-group-has-work? group))
                             (with-mutex
                               (async-scheduler-remote-mutex sched)
                               (async-queue-empty?
                                 (async-scheduler-remote-queue sched)))))])
                    (when idle? (poll sched #t)))
                  (let ([idle?
                         (with-mutex (async-scheduler-remote-mutex sched)
                           (async-queue-empty?
                             (async-scheduler-remote-queue sched)))])
                    (when idle? (poll sched #t))))))]
      [(async-group-parallel? ($async-scheduler-group sched))
       (let ([group ($async-scheduler-group sched)]
             [timer (async-timer-heap-peek (async-scheduler-timers sched))])
         (with-mutex (async-scheduler-remote-mutex sched)
           (when (and (async-queue-empty? (async-scheduler-remote-queue sched))
                      (with-mutex (async-scheduler-group-mutex group)
                        (and
                          (async-queue-empty?
                            (async-scheduler-group-ready-queue group))
                          (not (async-group-has-work? group))
                          (not (async-scheduler-group-shutdown? group)))))
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
                                   timeout))))))]
      [else
       (if-feature pthreads
         (let ([timer (async-timer-heap-peek (async-scheduler-timers sched))])
           (with-mutex (async-scheduler-remote-mutex sched)
             (when (async-queue-empty? (async-scheduler-remote-queue sched))
               (if (not timer)
                   (condition-wait (async-scheduler-remote-cond sched)
                                   (async-scheduler-remote-mutex sched))
                   (let* ([deadline (async-timer-deadline timer)]
                          [delta (max 0 (fx- deadline (async-monotonic-us)))]
                          [timeout (add-duration (current-time)
                                     (make-time 'time-duration
                                       (* (remainder delta 1000000) 1000)
                                       (quotient delta 1000000)))])
                     (condition-wait (async-scheduler-remote-cond sched)
                                     (async-scheduler-remote-mutex sched)
                                     timeout))))))
         (let ([timer (async-timer-heap-peek (async-scheduler-timers sched))])
           (when timer
             (let* ([deadline (async-timer-deadline timer)]
                    [delta (max 0 (fx- deadline (async-monotonic-us)))])
               (sleep (make-time 'time-duration
                        (* (remainder delta 1000000) 1000)
                        (quotient delta 1000000)))))))]))
      (lambda () (async-group-unmark-idle! sched)))))

(define async-preemption-token (list 'async-preempted))
(define async-minimum-preemption-ticks 1000)

(define call-with-async-task-prompt
  (lambda (sched thunk)
    (let ([outcome
           (#3%call/1cc
             (lambda (exit)
               (async-scheduler-preemption-exit-set! sched exit)
               ($control-reset-at
                 (async-scheduler-prompt-tag sched) #t thunk)))])
      (async-scheduler-preemption-exit-set! sched #f)
      outcome)))

;;; The timer handler runs at a Scheme safe point inside the scheduler prompt.
;;; It captures only the current fiber, publishes no wait, and returns a token
;;; to the scheduler.  Resumption returns here and continues the interrupted
;;; computation.  The handler is scheduler-independent so a task snapshot can
;;; migrate to another worker without retaining the old worker's state.
(define async-preemption-handler
  (lambda ()
    (let* ([sched ($async-scheduler)]
           [task
            (and (async-scheduler? sched)
                 (async-scheduler-current-task sched))])
      (unless (and task
                   (eq? (async-task-state task) 'running)
                   (async-scheduler-preemption-ticks sched))
        ($oops 'async-preemption-handler
          "timer interrupt outside a running preemptive task"))
      (let ([ticks (async-scheduler-preemption-ticks sched)])
        (if (not ($control-native1-capture-ready-at?
                   (async-scheduler-prompt-tag sched)))
            ;; An ordinary delimited-control transfer owns the continuation
            ;; while its shift handler may splice it. Finish that atomic extent
            ;; before splicing the surrounding scheduler prompt.
            (set-timer ticks)
            (begin
              ;; Cancellation of CPU-bound code is observed at the same native
              ;; safe point as preemption, without first capturing another
              ;; continuation.
              (async-check-cancellation! task)
              (snapshot-task-exception-state! sched task)
              (set-sched-switch! sched #t)
              ;; set-timer directly consumes the value returned when the
              ;; continuation is resumed.  This is the final operation in the
              ;; handler, so the timer cannot expire in a Scheme resumption
              ;; wrapper before user code makes progress.
              (set-timer
                (let* ([resume-info
                        (async-shift1-to-scheduler sched
                          (lambda (resumption)
                            (with-async-mutex (async-task-mutex task)
                              (async-invariant
                                (not (async-task-resumption task))
                                "preempted task already has a resumption" task)
                              (async-task-resumption-set! task resumption))
                            async-preemption-token))]
                       [resume-sched
                        (and (pair? resume-info) (car resume-info))]
                       [ticks
                        (and (pair? resume-info) (cdr resume-info))])
                  ;; A migrated continuation reinstates its captured thread
                  ;; parameters. Rebind the receiving scheduler before task
                  ;; code can observe scheduler-local state.
                  (unless (async-scheduler? resume-sched)
                    ($oops 'async-preemption-handler
                      "invalid preemption resume scheduler ~s" resume-sched))
                  ($async-scheduler resume-sched)
                  (set-current-sched-switch! #f)
                  ;; Cancellation can win while the captured task is ready but
                  ;; not running. Observe it directly on reentry instead of
                  ;; manufacturing a nested timer trap.
                  (async-check-cancellation! task)
                  (unless (and (fixnum? ticks) (fx> ticks 0))
                    ($oops 'async-preemption-handler
                      "invalid preemption tick budget ~s" ticks))
                  ticks))))))))

;;; Reserve the Chez tick timer only while user task code is active.  The
;;; scheduler loop, queue operations, polling, and callbacks run with their
;;; ambient handler and timer.  An enclosing engine is rejected by run-async,
;;; since both facilities own the same per-thread timer.
(define call-with-async-preemption
  (lambda (sched thunk)
    (let* ([saved-handler (timer-interrupt-handler)]
           [saved-ticks (set-timer 0)])
      (dynamic-wind
        (lambda ()
          (timer-interrupt-handler async-preemption-handler))
        (lambda ()
          ;; Timer ownership remains outside the task's private prompt.
          (call-with-async-task-prompt sched thunk))
        (lambda ()
          (set-timer 0)
          (timer-interrupt-handler saved-handler)
          (set-timer saved-ticks))))))

;;; Enter only the user task continuation under the scheduler prompt.
(define async-run-task-step
  (lambda (sched task)
    (if (async-task-entry task)
        (let ([entry (async-task-entry task)])
          (async-task-entry-set! task #f)
          (when (async-scheduler-preemption-ticks sched)
            (set-timer (async-scheduler-preemption-ticks sched)))
          (entry))
        (let ([resumption (async-task-resumption task)]
              [payload (async-task-payload task)]
              [preempted? (async-task-preempted? task)])
          (async-invariant
            (if preempted?
                ($control-native1-capture? resumption)
                (procedure? resumption))
            "ready task has no valid one-shot resumption" task)
          ;; Invocation transfers ownership of the one-shot continuation.
          (async-task-resumption-set! task #f)
          (async-task-preempted?-set! task #f)
          (async-task-payload-set! task #f)
          ;; A preemption resumption rearms after it reaches the interrupted
          ;; point.  Other resumptions rearm here because they do not pass
          ;; through the timer handler.
          (when (and (async-scheduler-preemption-ticks sched)
                     (not preempted?))
            (set-timer (async-scheduler-preemption-ticks sched)))
          ;; Rewinding captured dynamic-winds is part of the scheduling
          ;; switch, not a user-level wind entry.
          (set-sched-switch! sched #t)
          (if preempted?
              ($control-resume-native1-at
                (async-scheduler-prompt-tag sched) resumption payload)
              (resumption payload))))))

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
    (async-scheduler-exec-count-set! sched (fx+ 1 (async-scheduler-exec-count sched)))
    ;; Selection and cancellation form one handshake.  Once the task is
    ;; marked running, a later cancellation is observed at its next explicit
    ;; cancellation point; it must not replace a normal resumption payload on
    ;; a foreign worker after the owner check has already passed.
    (let-values ([(wait-owner cancel-at-selection? reroute?)
                  (with-async-mutex (async-task-mutex task)
                    (let* ([wait-owner (async-task-wait-scheduler task)]
                           [cancel? (task-cancel-requested? task)]
                           [reroute?
                            (and wait-owner
                                 (not (eq? wait-owner sched))
                                 (async-task-resumption task)
                                 cancel?)])
                      (unless reroute?
                        (async-task-current-wait-set! task #f)
                        (async-task-wait-scheduler-set! task #f)
                        (async-task-resume-pinned?-set! task #f)
                        (async-task-suspension-state-set! task #f))
                      (values wait-owner cancel? reroute?)))])
      (if reroute?
          (begin
            ;; A normal completion may have published the task globally just
            ;; before cancellation.  Return it to the suspension owner before
            ;; replacing the value payload with a cancellation raise.
            (async-task-scheduler-set! task wait-owner)
            (async-remote-submit wait-owner task)
            #f)
          (begin
            (if (and (async-task-entry task) cancel-at-selection?)
                ;; A ready task observes cancellation before running user code.
              (begin
                (terminate-task! sched task 'canceled
                  (cons 'raise (task-cancellation-condition task)))
                #f)
              (begin
                (when (and (not (async-task-entry task))
                           cancel-at-selection?
                           (not (async-task-cancel-shield? task)))
                  (unless (async-task-preempted? task)
                    (async-task-payload-set! task
                      (cons 'raise (task-cancellation-condition task)))))
                (when (async-task-preempted? task)
                  ;; The receiving worker is intentionally selected here, not
                  ;; when the continuation was captured, because the task may
                  ;; have moved through the shared work queues in between.
                  (async-task-payload-set! task
                    (cons sched
                      (async-scheduler-preemption-ticks sched))))
                (async-task-state-set! task 'running)
                (async-scheduler-current-task-set! sched task)
                (install-task-exception-state! sched task)
                (let* ([ticks (async-scheduler-preemption-ticks sched)]
                       [outcome
                        (guard (c [else (cons 'internal-escape c)])
                          (if ticks
                              (call-with-async-preemption sched
                                (lambda () (async-run-task-step sched task)))
                              (call-with-async-task-prompt sched
                                (lambda ()
                                  (async-run-task-step sched task)))))])
                  (set-sched-switch! sched #f)
                  (async-scheduler-preemption-exit-set! sched #f)
                  (async-scheduler-current-task-set! sched #f)
                  (restore-scheduler-exception-state! sched)
                  (cond
                    [(eq? outcome async-suspend-token)
                     (async-finish-suspension! task)
                     task]
                    [(eq? outcome async-preemption-token)
                     (async-task-preempted?-set! task #t)
                     ;; The timer handler consumes this value only after the
                     ;; one-shot continuation has been spliced onto its new
                     ;; worker.  Cancellation may replace it with a raise.
                     (async-task-payload-set! task ticks)
                     (async-task-state-set! task 'ready)
                     (async-scheduler-preemption-count-set! sched
                       (fx+ 1 (async-scheduler-preemption-count sched)))
                     (async-publish-ready! task sched)
                     #f]
                    [(and (pair? outcome) (eq? (car outcome) 'done))
                     ;; A task that observes cancellation and still returns
                     ;; has handled the request cooperatively.
                     (terminate-task! sched task 'completed outcome)
                     #f]
                    [(and (pair? outcome) (eq? (car outcome) 'failed))
                     (if ($async-cancellation-condition? (cdr outcome))
                         (terminate-task! sched task 'canceled outcome)
                         (terminate-task! sched task 'failed outcome))
                     #f]
                    [else
                     (terminate-task! sched task 'failed
                       (cons 'raise (cdr outcome)))
                     #f])))))))))

(define async-scheduler-run
  (lambda (sched)
    (let ([current (async-scheduler-current-queue sched)]
          [next (async-scheduler-next-queue sched)])
      (let loop ()
        (async-debug-check-owner! sched)
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

(define async-run-scheduler-thread
  (lambda (sched)
    (let ([old-sched ($async-scheduler)]
          [group ($async-scheduler-group sched)])
      (guard (c [else (async-group-fail! group c)])
        (dynamic-wind
          (lambda ()
            ($async-scheduler sched)
            (async-scheduler-status-set! sched 'running)
            (async-scheduler-owner-thread-set! sched (get-thread-id)))
          (lambda () (async-scheduler-run sched))
          (lambda ()
            ($async-io-shutdown sched)
            (async-scheduler-status-set! sched 'shutdown)
            (async-scheduler-owner-thread-set! sched #f)
            ($async-scheduler old-sched)))))))


;;; -------------------------------------------------------------- observability








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
    (perform-operation (group-empty-operation grp))
    (let loop ()
      (let ([u
             (with-async-mutex (async-task-group-mutex grp)
               (and (pair? (async-task-group-unobserved grp))
                    (let ([u (car (async-task-group-unobserved grp))])
                      (async-task-group-unobserved-set! grp
                        (cdr (async-task-group-unobserved grp)))
                      u)))])
        (when u
          (if (async-task-observed? (car u))
              (loop)
              (raise (cdr u))))))
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
    (perform-operation (async-fiber-mutex-acquire-operation mutex))))

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
    (perform-operation (async-rw-mutex-acquire-operation/raw mutex 'write))))

(set-who! async-rw-mutex-read-acquire
  (lambda (mutex)
    (unless (async-rw-mutex? mutex)
      ($oops who "~s is not an async rw mutex" mutex))
    (perform-operation (async-rw-mutex-acquire-operation/raw mutex 'read))))

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
    (perform-operation (async-wait-group-wait-operation/raw group))))

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
        (async-check-cancellation! task)
        (let* ([context (and (not (async-task-cancel-shield? task))
                             (async-current-context))]
               [ss (make-async-sync-state)])
          (when (and context (async-context-canceled?/raw context))
            (raise (async-context-cancellation-condition context)))
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
        (let ([state (unbox (async-future-state f))])
          (and (pair? state) (eq? (car state) 'done) (cdr state))))
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
    (perform-operation (future-operation f))))

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

(set-who! channel-put-operation
  (lambda (ch v)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (let ([token (list 'channel-put-operation)])
      (make-async-operation
        (lambda (ss)
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
            payload))
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
                                    (cons 'values
                                      (list (vector-ref putter 0) #t))
                                    (async-delivery-prepare-all!
                                      (list (vector-ref putter 2)))))]
                            [(async-channel-closed? ch)
                             (values
                               (async-channel-receive-closed-payload) '())]
                            [else (values #f '())]))])
            (async-delivery-publish-all! publications)
            payload))
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
    (perform-operation (channel-put-operation ch v))
    (void)))

(set-who! channel-get
  (lambda (ch)
    (perform-operation (channel-get-operation ch))))

(set-who! channel-receive
  (lambda (ch)
    (perform-operation (channel-receive-operation ch))))

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
                 [parent-context
                  (or context
                      (and group (async-task-group-context group))
                      (async-current-context)
                      (async-task-context parent))]
                 [task (async-make-task sched name grp parent-context migratable?
                         termination-actions
                         (lambda () (run-task-entry thunk)))])
            (when (and group parent)
              ;; propagate cancellation toward explicitly grouped tasks
              (with-async-mutex (async-task-group-mutex group)
                (unless (async-task-group-parent group)
                  (async-task-group-parent-set! group (ensure-child-group parent)))
                (let ([pg (async-task-group-parent group)])
                  (with-async-mutex (async-task-group-mutex pg)
                    (unless (memq group (async-task-group-subgroups pg))
                      (async-task-group-subgroups-set! pg
                        (cons group (async-task-group-subgroups pg))))))))
            (with-async-mutex (async-task-group-mutex grp)
              (hashtable-set! (async-task-group-children grp) task task)
              (async-task-group-child-count-set! grp
                (fx+ 1 (async-task-group-child-count grp))))
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
    (let ([sched ($async-scheduler)])
      (when (and (async-scheduler? sched)
                 (eq? task (async-scheduler-current-task sched)))
        ($oops who "a task cannot join itself")))
    (perform-operation (async-task-join-operation task))))

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
          (let ([op
                 (make-async-operation
                   (lambda (ss) #f)
                   (lambda (ss deliver)
                     (deliver (cons 'values (list #t)))
                     '(yield))
                   (lambda (vals) vals)
                   (lambda (ss) (void)))])
            (perform-operation op)
            #t)
          #f))))

(set-who! async-dynamic-wind
  (lambda (before thunk after)
    (unless (procedure? before) ($oops who "~s is not a procedure" before))
    (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
    (unless (procedure? after) ($oops who "~s is not a procedure" after))
    (dynamic-wind
      (lambda () (unless ($async-scheduling-switch?) (before)))
      thunk
      (lambda () (unless ($async-scheduling-switch?) (after))))))

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
      (let ([outer ($async-scheduler)])
        (when (and (async-scheduler? outer)
                   (async-scheduler-current-task outer)
                   (async-scheduler-preemption-ticks outer))
          ($oops who
            "cannot nest an async scheduler inside a preemptive task")))
      ;; Engines and async preemption both own the native thread's timer.
      (when ($engine-active?)
        ($oops who "cannot run an async scheduler inside an active engine"))
      (when (and (eq? clock 'virtual) (fx> parallelism 1))
        ($oops who "parallel scheduler groups require a real clock"))
      (when (fx> parallelism 1)
        (if-feature pthreads
          (void)
          ($oops who "parallel scheduler groups require thread support")))
      (let* ([group (make-async-scheduler-group
                      (control:make-continuation-prompt-tag 'async-scheduler-group)
                      (make-async-os-mutex)
                      (if-feature pthreads (make-condition) #f)
                      (make-async-queue) (make-eq-hashtable) 0
                      '#() #f 0 '() #f #f '())]
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
                ($async-scheduler sched)
                (async-scheduler-status-set! sched 'running)
                (async-scheduler-owner-thread-set! sched (get-thread-id)))
              (lambda () (async-scheduler-run sched))
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
    (let ([group ($async-scheduler-group sched)])
      (with-async-mutex (async-scheduler-group-mutex group)
        (async-scheduler-group-task-count group)))))

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
(set! $async-task-active?
  (lambda ()
    (let ([sched ($async-scheduler)])
      (and (async-scheduler? sched)
           (async-scheduler-current-task sched)
           #t))))
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
)
