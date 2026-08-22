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
;;; A scheduler owns a private continuation-prompt tag and runs lightweight
;;; tasks on one operating-system thread.  Tasks suspend with shift0-at and
;;; are resumed through one-shot resumptions.  Waitables (timers, channels,
;;; futures, joins) are expressed as operations with try/block/wrap/nack
;;; components.  The cooperative scheduler optionally bounds user task turns
;;; with Chez Scheme engines.  libuv-backed I/O plugs in through the
;;; scheduler's io fields (asyncio.ss).

;;; ----------------------------------------------------------- utilities

(let ()  ; private scope: public names are assigned to their declared globals

(define-syntax with-async-mutex
  (lambda (x)
    (syntax-case x ()
      [(_ m e1 e2 ...)
       (if-feature pthreads
         #'(with-mutex m e1 e2 ...)
         #'(begin e1 e2 ...))])))

(define make-async-mutex
  (lambda () (if-feature pthreads (make-mutex) #f)))

(define async-debug-invariants?
  (let ([v (getenv "CHEZ_ASYNC_CHECK_INVARIANTS")])
    (and v (not (member v '("" "0" "false" "no"))))))

(define async-debug-runnable-mutex (make-async-mutex))
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
;;; The current scheduler lives in a thread parameter.  It is re-installed
;;; after every dynamic-state swap so that a task's snapshot can never
;;; replace scheduler invariants.

(define $async-scheduler
  ($make-thread-parameter #f (lambda (x) x)))

;;; True only while a preemptive scheduler is executing a task engine.  It is
;;; scheduler-owned state rather than fiber dynamic state, and prevents a
;;; nested run-async from attempting to nest engines.
(define $async-engine-active
  ($make-thread-parameter #f (lambda (x) x)))

;;; A dynamically scoped context override is part of the fiber's saved thread
;;; parameter state.  Tasks still retain their own immutable context so task
;;; cancellation never cancels a caller-supplied shared context.
(define $async-context-override
  (make-thread-parameter #f))

;;; -------------------------------------------------------- sync states
;;;
;;; A sync state owns an atomic state box holding one of:
;;;   'waiting                 no claim yet
;;;   'claimed                 transient: a completer owns it
;;;   (done . payload)         final; payload = (values . vals) | (raise . c)
;;;
;;; Completion, cancellation, and failure compete with box-cas!.

(define-record-type (async-sync-state make-async-sync-state% async-sync-state?)
  (nongenerative async-sync-state-layer6)
  (sealed #t)
  (fields
    (immutable state)               ; atomic box: waiting | claimed | (done . payload)
    (immutable mutex)               ; protects registration metadata and slots
    (mutable registration-phase)    ; new | registering | registered
    (mutable cancel-pending?)
    (mutable nack)
    (immutable slots)))             ; operation token -> per-perform state

(define make-async-sync-state
  (lambda ()
    (make-async-sync-state% (box 'waiting) (make-async-mutex)
      'new #f #f (make-eq-hashtable))))

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
      (hashtable-set! (async-sync-state-slots ss) token value))))

(define async-sync-slot-ref
  (lambda (ss token default)
    (with-async-mutex (async-sync-state-mutex ss)
      (hashtable-ref (async-sync-state-slots ss) token default))))

(define async-sync-slot-delete!
  (lambda (ss token)
    (with-async-mutex (async-sync-state-mutex ss)
      (hashtable-delete! (async-sync-state-slots ss) token))))

;;; ------------------------------------------------------------ records

(define-record-type (async-scheduler make-async-scheduler% $async-scheduler?)
  (nongenerative async-scheduler-layer7)
  (sealed #t)
  (fields
    (immutable prompt-tag)          ; private continuation-prompt tag
    (immutable group $async-scheduler-group) ; owning scheduler group
    (immutable group-index)         ; stable index within the group
    (immutable current-queue)       ; tasks run this turn
    (immutable next-queue)          ; tasks run next turn
    (mutable work-deque)            ; migratable tasks; owner front, thieves back
    (immutable work-mutex)
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
    (mutable timers)                ; sorted list of async-timer
    (mutable current-task)          ; running task or #f
    (mutable in-switch?)            ; #t while unwinding/rewinding a switch
    (mutable saved-dynamic-state)   ; ambient async-dynamic-state
    (mutable active-dynamic-version); version installed on the owner thread
    (mutable saved-exception-state) ; ambient exception state
    (mutable turn-count $async-scheduler-turn-count $async-scheduler-turn-count-set!)
    (mutable exec-count)
    (immutable preemption-ticks)   ; #f for cooperative scheduling
    (mutable preemption-count)
    (mutable suspension-count $async-scheduler-suspension-count $async-scheduler-suspension-count-set!)
    (mutable wakeup-count $async-scheduler-wakeup-count $async-scheduler-wakeup-count-set!)
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
    (immutable parameter-config)    ; atomic box of versioned updates
    (mutable schedulers)
    (mutable root-task)
    (mutable next-task-id)
    (mutable next-wake-index)
    (mutable work-count)
    (mutable shutdown?)
    (mutable failure)
    (mutable workers)))

(define-record-type (async-timer make-async-timer async-timer?)
  (nongenerative)
  (sealed #t)
  (fields (immutable deadline) (immutable deliver-box)))

(define-record-type (async-context make-async-context% $async-context?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable parent)
    (immutable mutex)
    (mutable canceled? async-context-canceled?/raw
             async-context-canceled?/raw-set!)
    (mutable reason async-context-reason/raw async-context-reason/raw-set!)
    (mutable children)
    (mutable waiters)                ; list of (ss . deliver)
    (mutable deadline-cancel)))

(define-record-type (async-context-cancel-result
                      make-async-context-cancel-result
                      async-context-cancel-result?)
  (nongenerative)
  (sealed #t)
  (fields (immutable context)))

(define-record-type (async-task make-async-task $async-task?)
  (nongenerative async-task-layer7)
  (sealed #t)
  (fields
    (immutable id)
    (immutable name)
    (mutable state)                 ; ready running waiting completed failed canceled
    (mutable entry)                 ; thunk before first run, #f after
    (mutable resumption)            ; resumption procedure while waiting/ready
    (mutable engine)                ; suspended engine after timed preemption
    (immutable scheduler-group)     ; scheduler group in which the task runs
    (mutable scheduler)             ; current or most recent execution scheduler
    (mutable wait-scheduler)        ; scheduler that owns the current wait
    (immutable migratable?)
    (mutable affinity-reasons)      ; scheduler-bound execution constraints
    (mutable suspension-state)      ; #f | unwinding | parking | parked | delivered
    (mutable dynamic-state)         ; saved async-dynamic-state
    (mutable exception-state)       ; exception-state record
    (mutable parent-group)          ; group this task belongs to
    (immutable context)             ; task-owned cancellation context
    (mutable child-group)           ; group owned by this task, or #f
    (mutable result-values)
    (mutable failure-condition)     ; failure or cancellation condition
    (mutable join-waiters)          ; list of (ss . deliver)
    (mutable observed?)             ; failure observed by a joiner
    (mutable cancel-state)          ; #f | requested | delivered
    (mutable cancel-condition)
    (mutable current-wait)          ; wait description or #f
    (mutable nack-thunk)            ; withdraws the current wait
    (mutable payload)               ; pending delivery for a ready task
    (mutable sync-state)            ; box of the current wait, or #f
    (mutable cancel-shield?)        ; #t in shielded internal waits
    (immutable mutex)))

(define-record-type (async-task-group make-async-task-group% $async-task-group?)
  (nongenerative)
  (sealed #t)
  (fields
    (mutable children)              ; tasks belonging to the group
    (mutable subgroups)             ; linked descendant groups
    (mutable waiters)               ; list of (ss . deliver)
    (mutable unobserved)            ; unobserved child failure conditions
    (mutable parent)                ; parent group or #f
    (immutable context)
    (immutable mutex)))

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






;;; ------------------------------------------------- dynamic state (fiber-local)

(define-record-type (async-dynamic-state make-async-dynamic-state async-dynamic-state?)
  (nongenerative)
  (sealed #t)
  (fields (immutable parameters) (immutable version)))

(define async-snapshot-dynamic-state
  (lambda (version)
    (make-async-dynamic-state
      (if-feature pthreads
        (with-tc-mutex
          (vector-copy ($tc-field 'parameters ($tc))))
        #f)
      version)))

(define async-install-dynamic-state/raw!
  (lambda (state)
    (if-feature pthreads
      (let ([snap (async-dynamic-state-parameters state)])
        (when snap
          (let ([cur ($tc-field 'parameters ($tc))])
            (if (fx< (vector-length cur) (vector-length snap))
                ($tc-field 'parameters ($tc) (vector-copy snap))
                (do ([i 0 (fx+ i 1)])
                    ((fx= i (vector-length snap)))
                  (vector-set! cur i (vector-ref snap i)))))))
      (void))))

(define async-extend-parameter-vector
  (lambda (parameters size)
    (if (or (not parameters) (fx>= (vector-length parameters) size))
        parameters
        (let ([new (make-vector size #f)])
          (do ([i 0 (fx+ i 1)]) ((fx= i (vector-length parameters)))
            (vector-set! new i (vector-ref parameters i)))
          new))))

(define async-parameter-config-ref
  (lambda (group)
    (let ([config-box (async-scheduler-group-parameter-config group)])
      (let loop ()
        (let ([config (unbox config-box)])
          (if (box-cas! config-box config config)
              config
              (loop)))))))

;;; ------------------------------------------------ cancellation contexts

(define async-current-context
  (lambda ()
    (or ($async-context-override)
        (let ([sched ($async-scheduler)])
          (and ($async-scheduler? sched)
               (let ([task (async-scheduler-current-task sched)])
                 (and task (async-task-context task))))))))

(define async-context-canceled?/locked
  (lambda (context)
    (async-context-canceled?/raw context)))

(define async-cancel-timer!
  (lambda (timer)
    (when timer
      (let* ([deliver-box (async-timer-deliver-box timer)]
             [deliver (unbox deliver-box)])
        (when deliver (box-cas! deliver-box deliver #f))))))

(define async-context-detach!
  (lambda (context)
    (let ([parent (async-context-parent context)])
      (when parent
        (with-async-mutex (async-context-mutex parent)
          (async-context-children-set! parent
            (remq context (async-context-children parent))))))))

(define async-context-cancel-core!
  (lambda (context reason)
    (let-values ([(won? children waiters cancel-timer)
                  (with-async-mutex (async-context-mutex context)
                    (if (async-context-canceled?/locked context)
                        (values #f '() '() #f)
                        (let ([children (async-context-children context)]
                              [waiters (async-context-waiters context)]
                              [cancel-timer
                               (async-context-deadline-cancel context)])
                          (async-context-canceled?/raw-set! context #t)
                          (async-context-reason/raw-set! context reason)
                          (async-context-children-set! context '())
                          (async-context-waiters-set! context '())
                          (async-context-deadline-cancel-set! context #f)
                          (values #t children waiters cancel-timer))))])
      (when won?
        (async-context-detach! context)
        (when cancel-timer (cancel-timer))
        (for-each
          (lambda (waiter) ((cdr waiter) (cons 'values '())))
          waiters)
        (for-each
          (lambda (child) (async-context-cancel-core! child reason))
          children))
      won?)))

(define async-make-context
  (lambda (parent)
    (let ([context
           (make-async-context% parent (make-async-mutex)
             #f #f '() '() #f)])
      (when parent
        (let-values ([(canceled? reason)
                      (with-async-mutex (async-context-mutex parent)
                        (if (async-context-canceled?/locked parent)
                            (values #t (async-context-reason/raw parent))
                            (begin
                              (async-context-children-set! parent
                                (cons context
                                  (async-context-children parent)))
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
                       (async-context-waiters-set! context
                         (cons (cons ss deliver)
                           (async-context-waiters context)))
                       #f)))])
          (if ready?
              (begin (deliver (cons 'values '())) #f)
              (list 'context context))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-context-mutex context)
          (async-context-waiters-set! context
            (let loop ([waiters (async-context-waiters context)])
              (cond
                [(null? waiters) '()]
                [(eq? (caar waiters) ss) (cdr waiters)]
                [else (cons (car waiters) (loop (cdr waiters)))]))))))))

(define async-context-choice
  (lambda (context op)
    (choice-operation op
      (wrap-operation (async-context-operation context)
        (lambda () (make-async-context-cancel-result context))))))

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

(define async-apply-parameter-config
  (lambda (state config)
    (let ([version (car config)]
          [old-version (async-dynamic-state-version state)])
      (let collect ([updates (cdr config)] [pending '()])
        (if (or (null? updates)
                (fx<= (car (car updates)) old-version))
            (let apply ([updates pending]
                        [parameters (async-dynamic-state-parameters state)])
              (if (null? updates)
                  (make-async-dynamic-state parameters version)
                  (let* ([update (car updates)]
                         [index (cadr update)]
                         [initval (caddr update)]
                         [size (cadddr update)]
                         [parameters
                          (async-extend-parameter-vector parameters size)])
                    (when parameters
                      (vector-set! parameters index initval))
                    (apply (cdr updates) parameters))))
            (collect (cdr updates) (cons (car updates) pending)))))))

(define async-normalize-dynamic-state
  (lambda (group state)
    (let ([config (async-parameter-config-ref group)])
      (if (fx= (car config) (async-dynamic-state-version state))
          state
          (async-apply-parameter-config state config)))))

(define async-normalize-and-install-dynamic-state!
  (lambda (group state)
    (if-feature pthreads
      ;; Parameter allocation and parameter-vector access are serialized by
      ;; the runtime's thread-context mutex.  Configuration publication itself
      ;; is atomic and never takes a Scheme mutex while this mutex is held.
      (with-tc-mutex
        (let ([state (async-normalize-dynamic-state group state)])
          (async-install-dynamic-state/raw! state)
          state))
      state)))

;;; Parameter updates are published as immutable versioned entries.  A saved
;;; fiber applies missed entries when it is next installed, including entries
;;; for reused slots.
(define async-new-thread-parameter
  (lambda (index initval size)
    (let ([sched ($async-scheduler)])
      (when ($async-scheduler? sched)
        (let ([group ($async-scheduler-group sched)])
          ;; This hook runs inside the runtime's thread-context mutex, after
          ;; the new slot has been initialized in every live thread context.
          ;; Allocations are serialized by that mutex.  Publishing through the
          ;; atomic box avoids a lock inversion with Scheme mutex bookkeeping,
          ;; which can itself acquire the thread-context mutex.
          (let* ([config-box (async-scheduler-group-parameter-config group)]
                 [config (async-parameter-config-ref group)]
                 [version (fx+ 1 (car config))]
                 [next
                  (cons version
                    (cons (list version index initval size) (cdr config)))])
            (unless (box-cas! config-box config next)
              ($oops 'make-thread-parameter
                "concurrent parameter publication under the thread-context mutex"))
            ;; The allocating task is already running with the new slot
            ;; initialized by the runtime.  Record the version on that task;
            ;; other snapshots advance lazily when their task is installed.
            (let ([task (async-scheduler-current-task sched)])
              (when task
                (async-task-dynamic-state-set! task
                  (async-apply-parameter-config
                    (async-task-dynamic-state task) next))))
            ;; The allocating thread has already installed this slot.
            (async-scheduler-active-dynamic-version-set! sched version)))))))

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

(define async-task-add-affinity!
  (lambda (task reason)
    (with-async-mutex (async-task-mutex task)
      (unless (memq reason (async-task-affinity-reasons task))
        (async-task-affinity-reasons-set! task
          (cons reason (async-task-affinity-reasons task)))))))

(define async-task-remove-affinity!
  (lambda (task reason)
    (with-async-mutex (async-task-mutex task)
      (async-task-affinity-reasons-set! task
        (remq reason (async-task-affinity-reasons task))))))

(define async-task-affine?
  (lambda (task)
    (pair? (async-task-affinity-reasons task))))

(define async-task-group-runnable?
  (lambda (task)
    (and (async-task-migratable? task)
         (not (async-task-affine? task))
         (async-group-parallel? (async-task-scheduler-group task)))))

(define sched-now
  (lambda (sched)
    (if (async-scheduler-virtual? sched)
        (async-scheduler-vtime sched)
        (async-monotonic-us))))

(define install-task-dynamic-state!
  (lambda (sched task)
    (let* ([engine-was-active? ($engine-active?)]
           [group ($async-scheduler-group sched)]
           [ambient
            (async-snapshot-dynamic-state
              (async-scheduler-active-dynamic-version sched))]
           [state (async-normalize-and-install-dynamic-state! group
                    (async-task-dynamic-state task))])
      (async-scheduler-saved-dynamic-state-set! sched ambient)
      (async-scheduler-saved-exception-state-set! sched (current-exception-state))
      (async-task-dynamic-state-set! task state)
      (async-scheduler-active-dynamic-version-set! sched
        (async-dynamic-state-version state))
      (unless engine-was-active? ($engine-reset-thread-state!))
      (current-exception-state (async-task-exception-state task)))
    ($async-engine-active #f)
    ($async-scheduler sched)))

(define snapshot-task-dynamic-state!
  (lambda (sched task)
    (async-task-dynamic-state-set! task
      (async-snapshot-dynamic-state
        (async-dynamic-state-version
          (async-task-dynamic-state task))))
    (async-task-exception-state-set! task (current-exception-state))))

(define restore-scheduler-dynamic-state!
  (lambda (sched)
      (let ([state
             (async-normalize-and-install-dynamic-state!
               ($async-scheduler-group sched)
               (async-scheduler-saved-dynamic-state sched))])
      (async-scheduler-saved-dynamic-state-set! sched state)
      (async-scheduler-active-dynamic-version-set! sched
        (async-dynamic-state-version state))
      (current-exception-state (async-scheduler-saved-exception-state sched)))
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
        (async-invariant (fx= (async-scheduler-group-work-count group) 0)
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

(define async-group-next-wake-target!
  (lambda (group)
    (with-async-mutex (async-scheduler-group-mutex group)
      (let* ([schedulers (async-scheduler-group-schedulers group)]
             [n (vector-length schedulers)]
             [i (async-scheduler-group-next-wake-index group)])
        (async-scheduler-group-next-wake-index-set! group
          (fxmod (fx+ i 1) n))
        (vector-ref schedulers i)))))

(define async-work-push!
  (lambda (sched task)
    (with-async-mutex (async-scheduler-work-mutex sched)
      (async-debug-queue-claim! task 'work)
      (async-scheduler-work-deque-set! sched
        (cons task (async-scheduler-work-deque sched)))
      ;; Publish the count before releasing the deque lock, so a thief cannot
      ;; remove the task and decrement an as-yet-unpublished count.
      (let ([group ($async-scheduler-group sched)])
        (with-async-mutex (async-scheduler-group-mutex group)
          (async-scheduler-group-work-count-set! group
            (fx+ 1 (async-scheduler-group-work-count group))))))))

(define async-work-count-down!
  (lambda (group)
    (with-async-mutex (async-scheduler-group-mutex group)
      (async-scheduler-group-work-count-set! group
        (fx- (async-scheduler-group-work-count group) 1))
      (async-invariant (fx>= (async-scheduler-group-work-count group) 0)
        "scheduler group work count became negative" group))))

(define async-work-pop!
  (lambda (sched)
    (let ([task
           (with-async-mutex (async-scheduler-work-mutex sched)
             (let ([work (async-scheduler-work-deque sched)])
               (and (pair? work)
                    (begin
                      (async-scheduler-work-deque-set! sched (cdr work))
                      (async-debug-queue-release! (car work))
                      (car work)))))])
      (when task
        (async-work-count-down! ($async-scheduler-group sched)))
      task)))

(define async-list-take-last
  (lambda (xs)
    (if (null? (cdr xs))
        (values (car xs) '())
        (let-values ([(last prefix) (async-list-take-last (cdr xs))])
          (values last (cons (car xs) prefix))))))

(define async-work-steal!
  (lambda (victim)
    (let ([task
           (with-async-mutex (async-scheduler-work-mutex victim)
             (let ([work (async-scheduler-work-deque victim)])
               (if (null? work)
                   #f
                   (let-values ([(task remaining)
                                 (async-list-take-last work)])
                     (async-scheduler-work-deque-set! victim remaining)
                     (async-debug-queue-release! task)
                     task))))])
      (when task
        (async-work-count-down! ($async-scheduler-group victim)))
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
                        (loop (fx+ offset 1)))))))))))

(define async-work-submit!
  (lambda (task preferred)
    (async-work-push! preferred task)
    (async-wake-scheduler
      (async-group-next-wake-target! (async-task-scheduler-group task)))))

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
               (let* ([schedulers (async-scheduler-group-schedulers group)]
                      [n (vector-length schedulers)]
                      [i (async-scheduler-group-next-wake-index group)])
                 (async-scheduler-group-next-wake-index-set! group
                   (fxmod (fx+ i 1) n))
                 (vector-ref schedulers i)))])
        (async-wake-scheduler target)))))

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
      (with-async-mutex (async-scheduler-remote-mutex completion-sched)
        ($async-scheduler-wakeup-count-set! completion-sched
          (fx+ 1 ($async-scheduler-wakeup-count completion-sched))))
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
(define $async-make-deliver
  (lambda (ss task)
    (lambda (payload)
      (if (async-sync-state-claim! ss)
          (begin
            (async-sync-state-complete! ss payload)
            ($async-deliver-task task payload)
            #t)
          #f))))

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
    (make-async-task-group% '() '() '() '() parent context
      (make-async-mutex))))

(define group-cancel-children!
  (lambda (grp reason)
    (async-context-cancel-core! (async-task-group-context grp) reason)
    (with-async-mutex (async-task-group-mutex grp)
      (for-each
        (lambda (t) (task-cancel! t reason))
        (async-task-group-children grp))
      (for-each
        (lambda (g) (group-cancel-children! g reason))
        (async-task-group-subgroups grp)))))

(define group-child-terminated!
  (lambda (grp task)
    (let ([waiters
           (with-async-mutex (async-task-group-mutex grp)
             (async-task-group-children-set! grp
               (remq task (async-task-group-children grp)))
             (when (eq? (async-task-state task) 'failed)
               (async-task-group-unobserved-set! grp
                 (cons (cons task (async-task-failure-condition task))
                       (async-task-group-unobserved grp))))
             (if (null? (async-task-group-children grp))
                 (let ([ws (async-task-group-waiters grp)])
                   (async-task-group-waiters-set! grp '())
                   ws)
                 '()))])
      (for-each (lambda (w) ((cdr w) (cons 'values '()))) waiters))))

(define group-empty-operation
  (lambda (grp)
    (make-async-operation
      (lambda (ss)
        (with-async-mutex (async-task-group-mutex grp)
          (and (null? (async-task-group-children grp))
               (cons 'values '()))))
      (lambda (ss deliver)
        (with-async-mutex (async-task-group-mutex grp)
          (if (null? (async-task-group-children grp))
              (begin (deliver (cons 'values '())) #f)
              (begin
                (async-task-group-waiters-set! grp
                  (cons (cons ss deliver) (async-task-group-waiters grp)))
                (list 'task-group)))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-task-group-mutex grp)
          (async-task-group-waiters-set! grp
            (let loop ([ws (async-task-group-waiters grp)])
              (cond
                [(null? ws) '()]
                [(eq? (caar ws) ss) (cdr ws)]
                [else (cons (car ws) (loop (cdr ws)))]))))))))



;;; -------------------------------------------------------------- suspension

(define async-suspend-token (list 'async-suspended))

;;; One checked suspension operation: capture through the scheduler prompt,
;;; transition running->waiting, publish the wait, return to the scheduler.
(define $async-suspend
  (lambda (sched task ss register!)
    (async-check-cancellation! task)
    (snapshot-task-dynamic-state! sched task)
    (set-sched-switch! sched #t)
    (let ([payload
            ($control-shift-at (async-scheduler-prompt-tag sched) #t
              (lambda (k)
                (set-sched-switch! sched #f)
                (with-async-mutex (async-task-mutex task)
                  (async-task-state-set! task 'waiting)
                  (async-task-resumption-set! task k)
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
    (call-with-values
      (lambda () (async-deliver-operation-payload op payload))
      (lambda vals
        (if (and (pair? vals)
                 (null? (cdr vals))
                 (async-context-cancel-result? (car vals)))
            (raise
              (async-context-cancellation-condition
                (async-context-cancel-result-context (car vals))))
            (apply values vals))))))

;;; --------------------------------------------------------------- operations







;;; ------------------------------------------------------------------ timers

(define async-schedule-timer!
  (lambda (sched deadline deliver)
    (let ([timer (make-async-timer deadline (box deliver))])
      (let loop ([ts (async-scheduler-timers sched)] [acc '()])
        (if (and (pair? ts) (<= (async-timer-deadline (car ts)) deadline))
            (loop (cdr ts) (cons (car ts) acc))
            (async-scheduler-timers-set! sched
              (append-reverse acc (cons timer ts)))))
      timer)))

(define append-reverse
  (lambda (ls tail)
    (if (null? ls) tail (append-reverse (cdr ls) (cons (car ls) tail)))))

(define async-fire-due-timers!
  (lambda (sched)
    (let ([now (sched-now sched)])
      (let loop ()
        (let ([ts (async-scheduler-timers sched)])
          (when (and (pair? ts)
                     (<= (async-timer-deadline (car ts)) now))
            (let ([timer (car ts)])
              (async-scheduler-timers-set! sched (cdr ts))
              (let* ([deliver-box (async-timer-deliver-box timer)]
                     [deliver (unbox deliver-box)])
                (when (and deliver (box-cas! deliver-box deliver #f))
                  (deliver (cons 'values '()))))
              (loop))))))))



;;; ----------------------------------------------------------------- futures


(define future-complete!
  (lambda (f payload)
    (let ([waiters
           (with-async-mutex (async-future-mutex f)
             (unless (box-cas! (async-future-state f) 'waiting 'claimed)
               ($oops 'future-fulfil! "future is already fulfilled"))
             (let ([waiters (async-future-waiters f)])
               (async-future-waiters-set! f '())
               (set-box! (async-future-state f) (cons 'done payload))
               waiters))])
      (for-each (lambda (w) ((cdr w) payload)) waiters))))





;;; ---------------------------------------------------------------- channels


(define async-waiter-dead?
  (lambda (ss) (not (async-sync-state-live? ss))))

(define async-channel-prune!
  (lambda (ch)
    ;; bounded pruning: drop dead waiters while scanning
    (async-channel-gets-set! ch
      (filter (lambda (w) (not (async-waiter-dead? (car w)))) (async-channel-gets ch)))
    (async-channel-puts-set! ch
      (filter (lambda (w) (not (async-waiter-dead? (cadr w)))) (async-channel-puts ch)))))

(define async-channel-closed-condition
  (lambda (ch)
    ($make-channel-closed-condition (async-channel-close-reason ch))))

(define async-channel-put-closed-payload
  (lambda (ch)
    (cons 'raise (async-channel-closed-condition ch))))

(define async-channel-receive-closed-payload
  (lambda () (cons 'values '(#f #f))))

;;; Deliver value to the first live getter; returns #t on rendezvous.
(define async-channel-deliver-to-getter!
  (lambda (ch v)
    (let loop ([gs (async-channel-gets ch)])
      (cond
        [(null? gs) (async-channel-gets-set! ch '()) #f]
        [(async-waiter-dead? (caar gs)) (loop (cdr gs))]
        [((cdar gs) (cons 'values (list v #t)))
         (async-channel-gets-set! ch (cdr gs))
         #t]
        [else (loop (cdr gs))]))))

;;; Take a value from the first live putter; returns #t on rendezvous.
(define async-channel-take-from-putter!
  (lambda (ch k)  ; k: (value -> boolean delivered?)
    (let loop ([ps (async-channel-puts ch)])
      (cond
        [(null? ps) (async-channel-puts-set! ch '()) #f]
        [(async-waiter-dead? (cadr (car ps))) (loop (cdr ps))]
        [else
         (let ([v (car (car ps))] [deliver (caddr (car ps))])
           (if (k v deliver)
               (begin (async-channel-puts-set! ch (cdr ps)) #t)
               (loop (cdr ps))))]))))

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
  (lambda (sched name parent-group parent-context migratable? entry)
    (let* ([group ($async-scheduler-group sched)]
           [id (with-async-mutex (async-scheduler-group-mutex group)
                 (let ([id (async-scheduler-group-next-task-id group)])
                   (async-scheduler-group-next-task-id-set! group (fx+ 1 id))
                   id))])
      (make-async-task id name 'ready entry #f #f group sched #f migratable? '() #f
        (async-snapshot-dynamic-state
          (async-scheduler-active-dynamic-version sched))
        (current-exception-state)
        parent-group (async-make-context parent-context) #f '() #f '() #f #f
        #f #f #f #f #f #f (make-async-mutex)))))

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
             (null? (async-task-group-children grp)))
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
    (let ([join-waiters
           (with-async-mutex (async-task-mutex task)
             (async-task-state-set! task state)
             (async-task-current-wait-set! task #f)
             (async-task-wait-scheduler-set! task #f)
             (async-task-suspension-state-set! task #f)
             (async-task-resumption-set! task #f)
             (async-task-engine-set! task #f)
             (case state
               [(completed) (async-task-result-values-set! task (cdr outcome))]
               [else (async-task-failure-condition-set! task (cdr outcome))])
             (let ([waiters (async-task-join-waiters task)])
               (when (and (eq? state 'failed) (pair? waiters))
                 (async-task-observed?-set! task #t))
               (async-task-join-waiters-set! task '())
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

(define task-join-operation
  (lambda (task)
    (make-async-operation
      (lambda (ss)
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
                       (async-task-join-waiters-set! task
                         (cons (cons ss deliver) (async-task-join-waiters task)))
                       #f)))])
          (if payload
              (begin (deliver payload) #f)
              (list 'join (async-task-id task)))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-task-mutex task)
          (async-task-join-waiters-set! task
            (let loop ([ws (async-task-join-waiters task)])
              (cond
                [(null? ws) '()]
                [(eq? (caar ws) ss) (cdr ws)]
                [else (cons (car ws) (loop (cdr ws)))]))))))))


;;; ------------------------------------------------------------- cancellation


;;; ------------------------------------------------------------------- yield


;;; --------------------------------------------------------- async dynamic-wind


;;; --------------------------------------------------------------- scheduler

(define async-make-scheduler
  (lambda (virtual? group index preemption-ticks)
    (make-async-scheduler%
      (async-scheduler-group-prompt-tag group)
      group index
      (make-async-queue) (make-async-queue)
      '() (make-async-mutex)
      (make-async-queue) (make-async-mutex)
      (if-feature pthreads (make-condition) #f)
      (make-eq-hashtable) 0 0 #f 'created virtual? 0 '() #f #f
      (async-snapshot-dynamic-state 0) 0 (current-exception-state)
      0 0 preemption-ticks 0 0 0 #f #f #f)))

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
    (cond
      [(async-scheduler-virtual? sched)
       (let ([ts (async-scheduler-timers sched)])
         (if (null? ts)
             ($oops 'run-async
               "async deadlock: no runnable tasks and no pending timers")
             (async-scheduler-vtime-set! sched (async-timer-deadline (car ts)))))]
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
                             (fx= (async-scheduler-group-work-count group) 0)
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
             [ts (async-scheduler-timers sched)])
         (with-mutex (async-scheduler-remote-mutex sched)
           (when (and (async-queue-empty? (async-scheduler-remote-queue sched))
                      (with-mutex (async-scheduler-group-mutex group)
                        (and
                          (async-queue-empty?
                            (async-scheduler-group-ready-queue group))
                          (fx= (async-scheduler-group-work-count group) 0)
                          (not (async-scheduler-group-shutdown? group)))))
             (if (null? ts)
                 (condition-wait (async-scheduler-remote-cond sched)
                                 (async-scheduler-remote-mutex sched))
                 (let* ([deadline (async-timer-deadline (car ts))]
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
         (let ([ts (async-scheduler-timers sched)])
           (with-mutex (async-scheduler-remote-mutex sched)
             (when (async-queue-empty? (async-scheduler-remote-queue sched))
               (if (null? ts)
                   (condition-wait (async-scheduler-remote-cond sched)
                                   (async-scheduler-remote-mutex sched))
                   (let* ([deadline (async-timer-deadline (car ts))]
                          [delta (max 0 (fx- deadline (async-monotonic-us)))]
                          [timeout (add-duration (current-time)
                                     (make-time 'time-duration
                                       (* (remainder delta 1000000) 1000)
                                       (quotient delta 1000000)))])
                     (condition-wait (async-scheduler-remote-cond sched)
                                     (async-scheduler-remote-mutex sched)
                                     timeout))))))
         (let ([ts (async-scheduler-timers sched)])
           (unless (null? ts)
             (let* ([deadline (async-timer-deadline (car ts))]
                    [delta (max 0 (fx- deadline (async-monotonic-us)))])
               (sleep (make-time 'time-duration
                        (* (remainder delta 1000000) 1000)
                        (quotient delta 1000000)))))))])))

(define async-engine-expiration-token (list 'async-engine-expired))

;;; Enter only the user task continuation under an engine.  Scheduler queue
;;; maintenance, polling, callbacks, and dynamic-state installation remain
;;; outside the engine and therefore cannot be preempted.
(define async-run-task-step
  (lambda (sched task)
    (if (async-task-entry task)
        (let ([entry (async-task-entry task)])
          (async-task-entry-set! task #f)
          ($control-reset-at (async-scheduler-prompt-tag sched) #t entry))
        (let ([resumption (async-task-resumption task)]
              [payload (async-task-payload task)])
          (async-task-payload-set! task #f)
          ;; Rewinding captured dynamic-winds is part of the scheduling
          ;; switch, not a user-level wind entry.
          (set-sched-switch! sched #t)
          (resumption payload)))))

(define async-run-task-engine
  (lambda (sched task ticks)
    (let* ([saved-engine (async-task-engine task)]
           [engine (or saved-engine
                       ($make-engine-with-timer-hooks
                         (lambda () (async-run-task-step sched task))
                         (lambda () (set-current-sched-switch! #t))
                         (lambda () (set-current-sched-switch! #f))))]
          [old-active ($async-engine-active)])
      (async-task-engine-set! task #f)
      (when saved-engine
        (async-task-remove-affinity! task 'engine))
      ($async-engine-active #t)
      (when saved-engine (set-sched-switch! sched #t))
      (let ([outcome
             (guard (c [else (cons 'internal-escape c)])
               (engine ticks
                 (lambda (remaining outcome) outcome)
                 (lambda (next-engine)
                   (cons async-engine-expiration-token next-engine))))])
        ($async-engine-active old-active)
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
    (with-async-mutex (async-scheduler-group-mutex
                        ($async-scheduler-group sched))
      (async-invariant
        (eq? (hashtable-ref
               (async-scheduler-group-tasks ($async-scheduler-group sched))
               (async-task-id task) #f)
             task)
        "scheduler selected a task missing from the registry" task))
    (async-scheduler-exec-count-set! sched (fx+ 1 (async-scheduler-exec-count sched)))
    (let ([wait-owner (async-task-wait-scheduler task)])
      (if (and wait-owner
               (not (eq? wait-owner sched))
               (async-task-resumption task)
               (task-cancel-requested? task))
          (begin
            ;; A normal completion may have published the task globally just
            ;; before cancellation.  Return it to the suspension owner before
            ;; replacing the value payload with a cancellation raise.
            (async-task-scheduler-set! task wait-owner)
            (async-remote-submit wait-owner task)
            #f)
          (begin
            (async-task-current-wait-set! task #f)
            (async-task-wait-scheduler-set! task #f)
            (async-task-suspension-state-set! task #f)
            (if (and (async-task-entry task) (task-cancel-requested? task))
                ;; A ready task observes cancellation before running user code.
              (begin
                (terminate-task! sched task 'canceled
                  (cons 'raise (task-cancellation-condition task)))
                #f)
              (begin
                (when (and (not (async-task-entry task))
                           (task-cancel-requested? task)
                           (not (async-task-cancel-shield? task)))
                  (async-task-payload-set! task
                    (cons 'raise (task-cancellation-condition task))))
                (async-task-state-set! task 'running)
                (async-scheduler-current-task-set! sched task)
                (install-task-dynamic-state! sched task)
                (let* ([ticks (async-scheduler-preemption-ticks sched)]
                       [outcome
                        (if ticks
                            (async-run-task-engine sched task ticks)
                            (guard (c [else (cons 'internal-escape c)])
                              (async-run-task-step sched task)))])
                  (when (and (pair? outcome)
                             (eq? (car outcome)
                                  async-engine-expiration-token))
                    (snapshot-task-dynamic-state! sched task))
                  (set-sched-switch! sched #f)
                  (async-scheduler-current-task-set! sched #f)
                  (restore-scheduler-dynamic-state! sched)
                  (cond
                    [(eq? outcome async-suspend-token)
                     (async-finish-suspension! task)
                     task]
                    [(and (pair? outcome)
                          (eq? (car outcome)
                               async-engine-expiration-token))
                     (async-task-engine-set! task (cdr outcome))
                     (async-task-add-affinity! task 'engine)
                     (async-task-state-set! task 'ready)
                     (async-scheduler-preemption-count-set! sched
                       (fx+ 1 (async-scheduler-preemption-count sched)))
                     (async-debug-queue-claim! task 'next)
                     (async-queue-push!
                       (async-scheduler-next-queue sched)
                       task)
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
    (let ([previous ($async-context-override)])
      (async-dynamic-wind
        (lambda () ($async-context-override context))
        thunk
        (lambda () ($async-context-override previous))))))

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
    (perform-operation (async-context-choice context op))))

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
                                            (lambda (payload)
                                              (let ([won?
                                                     (deliver
                                                       (if (eq? (car payload) 'values)
                                                           (cons 'values
                                                             (list
                                                               (make-async-choice-result
                                                                 op (cdr payload))))
                                                           payload))])
                                                (when won?
                                                  (do ([j 0 (fx+ j 1)])
                                                      ((fx= j n))
                                                    (unless (fx= i j)
                                                      ((operation-nack
                                                         (vector-ref ops j))
                                                       ss))))
                                                won?))))])
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
               [op (if context (async-context-choice context op) op)]
               [ss (make-async-sync-state)])
          (let ([r ((operation-try op) ss)])
            (if r
                (async-deliver-operation-result op r)
                (async-deliver-operation-result op
                  ($async-suspend sched task ss
                    (lambda (ss*)
                      (let ([nack (lambda () ((operation-nack op) ss))])
                        (async-task-nack-thunk-set! task nack)
                        (async-sync-begin-registration! ss nack)
                        (let ([desc ((operation-block op) ss
                                      ($async-make-deliver ss task))])
                          (when (async-sync-end-registration! ss)
                            ($async-cancel-waiting-task task))
                          desc))))))))))))

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
              (let* ([deliver-box (async-timer-deliver-box timer)]
                     [deliver (unbox deliver-box)])
                (when deliver (box-cas! deliver-box deliver #f))))))))))

(set-who! async-sleep
  (lambda (seconds)
    (unless (async-valid-seconds? seconds)
      ($oops who "~s is not a nonnegative real number" seconds))
    (perform-operation (sleep-operation seconds))
    (void)))

(set-who! make-future
  (lambda ()
    (make-async-future% (box 'waiting) '() (make-async-mutex))))

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
                  (async-future-waiters-set! f (cons (cons ss deliver) (async-future-waiters f)))
                  (list 'future))))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-future-mutex f)
          (async-future-waiters-set! f
            (let loop ([ws (async-future-waiters f)])
              (cond
                [(null? ws) '()]
                [(eq? (caar ws) ss) (cdr ws)]
                [else (cons (car ws) (loop (cdr ws)))]))))))))

(set-who! future-get
  (lambda (f)
    (perform-operation (future-operation f))))

(set-who! make-channel
  (case-lambda
    [() (make-async-channel% 0 (make-async-mutex) #f 0 0 '() '() #f #f)]
    [(capacity)
     (unless (and (fixnum? capacity) (fx>= capacity 0))
       ($oops who "~s is not a nonnegative fixnum" capacity))
     (make-async-channel% capacity (make-async-mutex)
       (and (fx> capacity 0) (make-vector capacity)) 0 0 '() '() #f #f)]))

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
                           (async-channel-prune! ch)
                           (async-invariant
                             (or (fx= (async-channel-bcount ch) 0)
                                 (null? (async-channel-gets ch)))
                             "buffered channel has a live receiver at close"
                             ch)
                           (let ([puts (async-channel-puts ch)]
                                 [gets (if (fx= (async-channel-bcount ch) 0)
                                           (async-channel-gets ch)
                                           '())])
                             (async-channel-puts-set! ch '())
                             (async-channel-gets-set! ch '())
                             (values puts gets)))))])
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
    (make-async-operation
      (lambda (ss)
        (with-async-mutex (async-channel-mutex ch)
          (async-channel-prune! ch)
          (if (async-channel-closed? ch)
              (async-channel-put-closed-payload ch)
              (if (async-channel-deliver-to-getter! ch v)
              (cons 'values '())
              (if (and (fx> (async-channel-capacity ch) 0)
                       (fx< (async-channel-bcount ch) (async-channel-capacity ch)))
                  (begin (async-buffer-push! ch v) (cons 'values '()))
                  #f)))))
      (lambda (ss deliver)
        (with-async-mutex (async-channel-mutex ch)
          (async-channel-prune! ch)
          (if (async-channel-closed? ch)
              (begin (deliver (async-channel-put-closed-payload ch)) #f)
              (if (async-channel-deliver-to-getter! ch v)
              (begin (deliver (cons 'values '())) #f)
              (if (and (fx> (async-channel-capacity ch) 0)
                       (fx< (async-channel-bcount ch) (async-channel-capacity ch)))
                  (begin
                    (async-buffer-push! ch v)
                    (deliver (cons 'values '()))
                    #f)
                  (begin
                    (async-channel-puts-set! ch
                      (append (async-channel-puts ch) (list (list v ss deliver))))
                    (list 'channel-put ch)))))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-channel-mutex ch)
          (async-channel-puts-set! ch
            (filter (lambda (p) (not (eq? (cadr p) ss)))
              (async-channel-puts ch))))))))

(set-who! channel-receive-operation
  (lambda (ch)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (make-async-operation
      (lambda (ss)
        (with-async-mutex (async-channel-mutex ch)
          (async-channel-prune! ch)
          (cond
            [(and (fx> (async-channel-capacity ch) 0)
                  (fx> (async-channel-bcount ch) 0))
             (let ([v (async-buffer-pop! ch)])
               ;; move a waiting put into the buffer
               (async-channel-take-from-putter! ch
                 (lambda (pv deliver)
                   (if (deliver (cons 'values '()))
                       (begin (async-buffer-push! ch pv) #t)
                       #f)))
               (cons 'values (list v #t)))]
            [else
             (let ([got (box #f)])
               (if (async-channel-take-from-putter! ch
                     (lambda (pv deliver)
                       (if (deliver (cons 'values '()))
                           (begin (set-box! got pv) #t)
                           #f)))
                   (cons 'values (list (unbox got) #t))
                   (and (async-channel-closed? ch)
                        (async-channel-receive-closed-payload))))])))
      (lambda (ss deliver)
        (with-async-mutex (async-channel-mutex ch)
          (async-channel-prune! ch)
          (let ([got (box #f)] [done? (box #f)])
            (cond
              [(and (fx> (async-channel-capacity ch) 0)
                    (fx> (async-channel-bcount ch) 0))
               (let ([v (async-buffer-pop! ch)])
                 (async-channel-take-from-putter! ch
                   (lambda (pv pdeliver)
                     (if (pdeliver (cons 'values '()))
                         (begin (async-buffer-push! ch pv) #t)
                         #f)))
                 (deliver (cons 'values (list v #t)))
                 #f)]
              [(async-channel-take-from-putter! ch
                 (lambda (pv pdeliver)
                   (if (pdeliver (cons 'values '()))
                       (begin (set-box! got pv) #t)
                       #f)))
               (deliver (cons 'values (list (unbox got) #t)))
               #f]
              [(async-channel-closed? ch)
               (deliver (async-channel-receive-closed-payload))
               #f]
              [else
               (async-channel-gets-set! ch
                 (append (async-channel-gets ch) (list (cons ss deliver))))
               (list 'channel-receive ch)]))))
      (lambda (vals) vals)
      (lambda (ss)
        (with-async-mutex (async-channel-mutex ch)
          (async-channel-gets-set! ch
            (filter (lambda (g) (not (eq? (car g) ss)))
              (async-channel-gets ch))))))))

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

(set-who! spawn-task
  (lambda (thunk . options)
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
              (async-task-group-children-set! grp
                (cons task (async-task-group-children grp))))
            (sched-registry-add! sched task)
            (if (and migratable?
                     (async-group-parallel? ($async-scheduler-group sched)))
                (async-work-submit! task sched)
                (begin
                  (async-debug-queue-claim! task 'next)
                  (async-queue-push! (async-scheduler-next-queue sched) task)))
            task))))))

(set-who! task-join
  (lambda (task)
    (unless (task? task) ($oops who "~s is not a task" task))
    (let ([sched ($async-scheduler)])
      (when (and (async-scheduler? sched)
                 (eq? task (async-scheduler-current-task sched)))
        ($oops who "a task cannot join itself")))
    (perform-operation (task-join-operation task))))

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
                           (async-task-cancel-state-set! task 'requested)
                           (async-task-cancel-condition-set! task
                             (make-async-cancellation-condition reason))
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
               (unless (or (not v) (and (fixnum? v) (fx> v 0)))
                 ($oops who "invalid preemption tick count ~s" v))
               (set! preemption-ticks v)]
              [else ($oops who "unrecognized run-async option ~s" k)]))
          (loop (cddr opts))))
      (when (or ($async-engine-active)
                (and preemption-ticks ($engine-active?)))
        ($oops who "cannot nest an async scheduler inside a task engine"))
      (when (and (eq? clock 'virtual) (fx> parallelism 1))
        ($oops who "parallel scheduler groups require a real clock"))
      (when (fx> parallelism 1)
        (if-feature pthreads
          (void)
          ($oops who "parallel scheduler groups require thread support")))
      (let* ([group (make-async-scheduler-group
                      (control:make-continuation-prompt-tag 'async-scheduler-group)
                      (make-async-mutex)
                      (if-feature pthreads (make-condition) #f)
                      (make-async-queue) (make-eq-hashtable) 0
                      (box (cons 0 '())) '#() #f 0 0 0 #f #f '())]
             [schedulers (make-vector parallelism)])
        (do ([i 0 (fx+ i 1)]) ((fx= i parallelism))
          (vector-set! schedulers i
            (async-make-scheduler
              (eq? clock 'virtual) group i preemption-ticks)))
        (async-scheduler-group-schedulers-set! group schedulers)
        (let* ([sched (vector-ref schedulers 0)]
               [root (async-make-task sched #f #f #f #f
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
    ($async-scheduler-wakeup-count sched)))

;;; ------------------------------------------------- io layer integration

;;; Hooks consumed by asyncio.ss.  $async-io-shutdown is replaced by the io
;;; layer's real shutdown procedure when that file is loaded.
(set! $async-new-thread-parameter async-new-thread-parameter)
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
  (lambda (sched) (async-scheduler-timers sched)))

(set! $async-scheduler-virtual?
  (lambda (sched) (async-scheduler-virtual? sched)))

(set! $async-timer-deadline
  (lambda (timer) (async-timer-deadline timer)))

(set! $async-monotonic-us
  (lambda () (async-monotonic-us)))
)
