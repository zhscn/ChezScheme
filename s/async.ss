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
;;; components.  Layer-1..3: cooperative scheduler, dynamic-state capture,
;;; operations/timers/futures/channels.  libuv-backed I/O plugs in through
;;; the scheduler's io fields (asyncio.ss).

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


;;; -------------------------------------------- scheduler-owned storage
;;;
;;; The current scheduler lives in a thread parameter.  It is re-installed
;;; after every dynamic-state swap so that a task's snapshot can never
;;; replace scheduler invariants.

(define $async-scheduler
  ($make-thread-parameter #f (lambda (x) x)))

;;; -------------------------------------------------------- sync states
;;;
;;; A sync state is a box holding one of:
;;;   'waiting                 no claim yet
;;;   'claimed                 transient: a completer owns it
;;;   (done . payload)         final; payload = (values . vals) | (raise . c)
;;;
;;; Completion, cancellation, and failure compete with box-cas!.

(define make-async-sync-state (lambda () (box 'waiting)))

(define async-sync-state-live?
  (lambda (ss) (eq? (unbox ss) 'waiting)))

;;; ------------------------------------------------------------ records

(define-record-type (async-scheduler make-async-scheduler% $async-scheduler?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable prompt-tag)          ; private continuation-prompt tag
    (immutable current-queue)       ; tasks run this turn
    (immutable next-queue)          ; tasks run next turn
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
    (mutable saved-dynamic-state)   ; ambient parameter vector
    (mutable saved-exception-state) ; ambient exception state
    (mutable turn-count $async-scheduler-turn-count $async-scheduler-turn-count-set!)
    (mutable exec-count)
    (mutable suspension-count $async-scheduler-suspension-count $async-scheduler-suspension-count-set!)
    (mutable wakeup-count $async-scheduler-wakeup-count $async-scheduler-wakeup-count-set!)
    (mutable wake-proc)             ; io layer wakeup hook, or #f
    (mutable poll-proc)             ; io layer poll hook: scheduler x block? -> void
    (mutable io-state)))            ; io layer data

(define-record-type (async-timer make-async-timer async-timer?)
  (nongenerative)
  (sealed #t)
  (fields (immutable deadline) (immutable deliver)))

(define-record-type (async-task make-async-task $async-task?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable id)
    (immutable name)
    (mutable state)                 ; ready running waiting completed failed canceled
    (mutable entry)                 ; thunk before first run, #f after
    (mutable resumption)            ; resumption procedure while waiting/ready
    (immutable scheduler)
    (immutable migratable?)
    (mutable dynamic-state)         ; saved parameter vector
    (mutable exception-state)       ; exception-state record
    (mutable parent-group)          ; group this task belongs to
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
    (mutable gets)))                ; list of (ss . deliver)






;;; ------------------------------------------------- dynamic state (fiber-local)

(define async-snapshot-dynamic-state
  (lambda ()
    (if-feature pthreads
      (vector-copy ($tc-field 'parameters ($tc)))
      #f)))

(define async-install-dynamic-state!
  (lambda (snap)
    (if-feature pthreads
      (when snap
        (let ([cur ($tc-field 'parameters ($tc))])
          (let ([n (fxmin (vector-length cur) (vector-length snap))])
            (do ([i 0 (fx+ i 1)]) ((fx= i n))
              (vector-set! cur i (vector-ref snap i))))))
      (void))))

(define async-set-dynamic-state-slot
  (lambda (snap index initval size)
    (and snap
         (let ([snap
                (if (fx< (vector-length snap) size)
                    (let ([new (make-vector size)])
                      (do ([i 0 (fx+ i 1)])
                          ((fx= i (vector-length snap)))
                        (vector-set! new i (vector-ref snap i)))
                      new)
                    snap)])
           (vector-set! snap index initval)
           snap))))

;;; A thread parameter allocated while fibers are active introduces a new or
;;; reused vector slot.  Give every saved fiber the parameter's initial value;
;;; the allocating fiber's subsequent mutations are captured normally.
(define async-new-thread-parameter
  (lambda (index initval size)
    (let ([sched ($async-scheduler)])
      (when ($async-scheduler? sched)
        (async-scheduler-saved-dynamic-state-set! sched
          (async-set-dynamic-state-slot
            (async-scheduler-saved-dynamic-state sched)
            index initval size))
        (let-values ([(ids tasks)
                      (hashtable-entries (async-scheduler-tasks sched))])
          (vector-for-each
            (lambda (task)
              (async-task-dynamic-state-set! task
                (async-set-dynamic-state-slot
                  (async-task-dynamic-state task)
                  index initval size)))
            tasks))))))

;;; ------------------------------------------- scheduler/task internal helpers




(define task-terminal?
  (lambda (task)
    (memq (async-task-state task) '(completed failed canceled))))

(define task-cancel-requested?
  (lambda (task) (and (async-task-cancel-state task) #t)))

(define task-cancellation-condition
  (lambda (task)
    (or (async-task-cancel-condition task)
        (make-async-cancellation-condition #f))))

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

(define $async-scheduling-switch?
  (lambda ()
    (let ([sched ($async-scheduler)])
      (and (async-scheduler? sched)
           (async-scheduler-in-switch? sched)))))

(define sched-now
  (lambda (sched)
    (if (async-scheduler-virtual? sched)
        (async-scheduler-vtime sched)
        (async-monotonic-us))))

(define install-task-dynamic-state!
  (lambda (sched task)
    (async-scheduler-saved-dynamic-state-set! sched (async-snapshot-dynamic-state))
    (async-scheduler-saved-exception-state-set! sched (current-exception-state))
    (async-install-dynamic-state! (async-task-dynamic-state task))
    (current-exception-state (async-task-exception-state task))
    ($async-scheduler sched)))

(define save-task-dynamic-state!
  (lambda (sched task)
    (async-task-dynamic-state-set! task (async-snapshot-dynamic-state))
    (async-task-exception-state-set! task (current-exception-state))
    (async-install-dynamic-state! (async-scheduler-saved-dynamic-state sched))
    (current-exception-state (async-scheduler-saved-exception-state sched))
    ($async-scheduler sched)))

(define sched-registry-add!
  (lambda (sched task)
    (hashtable-set! (async-scheduler-tasks sched) (async-task-id task) task)
    ($async-scheduler-task-count-set! sched (fx+ 1 ($async-scheduler-task-count sched)))))

(define sched-registry-remove!
  (lambda (sched task)
    (hashtable-delete! (async-scheduler-tasks sched) (async-task-id task))
    ($async-scheduler-task-count-set! sched (fx- ($async-scheduler-task-count sched) 1))))

;;; Deliver a payload to a ready task.  May run on a foreign thread.
(define $async-deliver-task
  (lambda (task payload)
    (let ([sched (async-task-scheduler task)])
      (async-task-payload-set! task payload)
      (async-task-current-wait-set! task #f)
      (async-task-nack-thunk-set! task #f)
      (async-task-state-set! task 'ready)
      ($async-scheduler-wakeup-count-set! sched (fx+ 1 ($async-scheduler-wakeup-count sched)))
      (if (and (async-scheduler-owner-thread sched)
               (fx= (async-scheduler-owner-thread sched) (get-thread-id)))
          (async-queue-push! (async-scheduler-next-queue sched) task)
          (async-remote-submit sched task)))))

(define async-remote-submit
  (lambda (sched task)
    (with-async-mutex (async-scheduler-remote-mutex sched)
      (async-queue-push! (async-scheduler-remote-queue sched) task))
    (async-wake-scheduler sched)))

(define async-wake-scheduler
  (lambda (sched)
    (let ([wake (async-scheduler-wake-proc sched)])
      (when wake (wake)))
    (if-feature pthreads
      (with-mutex (async-scheduler-remote-mutex sched)
        (condition-broadcast (async-scheduler-remote-cond sched)))
      (void))))

;;; The single claim point for completing a suspended task.  Returns #t when
;;; the claim succeeded.
(define $async-make-deliver
  (lambda (ss task)
    (lambda (payload)
      (if (box-cas! ss 'waiting 'claimed)
          (begin
            (set-box! ss (cons 'done payload))
            ($async-deliver-task task payload)
            #t)
          #f))))

;;; Cancellation of a waiting task: claim, nack, resume with condition.
(define $async-cancel-waiting-task
  (lambda (task)
    (let ([ss (async-task-sync-state task)])
      (when (and ss (box-cas! ss 'waiting 'claimed))
        (let ([nack (async-task-nack-thunk task)])
          (when nack (nack)))
        (let ([payload (cons 'raise (task-cancellation-condition task))])
          (set-box! ss (cons 'done payload))
          ($async-deliver-task task payload))))))

;;; ------------------------------------------------------------- task groups

(define make-async-group
  (lambda (parent)
    (make-async-task-group% '() '() '() '() parent (make-async-mutex))))

(define group-cancel-children!
  (lambda (grp reason)
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
      (lambda (ss) (void)))))



;;; -------------------------------------------------------------- suspension

(define async-suspend-token (list 'async-suspended))

;;; One checked suspension operation: capture through the scheduler prompt,
;;; transition running->waiting, publish the wait, return to the scheduler.
(define $async-suspend
  (lambda (sched task ss register!)
    (async-check-cancellation! task)
    (set-sched-switch! sched #t)
    (let ([payload
            ($control-shift-at (async-scheduler-prompt-tag sched) #t
              (lambda (k)
                (set-sched-switch! sched #f)
                (async-task-state-set! task 'waiting)
                (async-task-resumption-set! task k)
                (async-task-sync-state-set! task ss)
                ($async-scheduler-suspension-count-set! sched
                  (fx+ 1 ($async-scheduler-suspension-count sched)))
                (let ([desc (register! ss)])
                  (when (and desc (eq? (async-task-state task) 'waiting))
                    (async-task-current-wait-set! task desc)))
                async-suspend-token))])
      (set-sched-switch! sched #f)
      payload)))

(define async-deliver-operation-payload
  (lambda (op payload)
    (if (eq? (car payload) 'values)
        (apply values ((operation-wrap op) (cdr payload)))
        (raise (cdr payload)))))

;;; --------------------------------------------------------------- operations







;;; ------------------------------------------------------------------ timers

(define async-schedule-timer!
  (lambda (sched deadline deliver)
    (let ([timer (make-async-timer deadline deliver)])
      (let loop ([ts (async-scheduler-timers sched)] [acc '()])
        (if (and (pair? ts) (<= (async-timer-deadline (car ts)) deadline))
            (loop (cdr ts) (cons (car ts) acc))
            (async-scheduler-timers-set! sched
              (append-reverse acc (cons timer ts))))))
    (void)))

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
              ((async-timer-deliver timer) (cons 'values '()))
              (loop))))))))



;;; ----------------------------------------------------------------- futures


(define future-complete!
  (lambda (f payload)
    (with-async-mutex (async-future-mutex f)
      (unless (box-cas! (async-future-state f) 'waiting 'claimed)
        ($oops 'future-fulfil! "future is already fulfilled"))
      (let ([waiters (async-future-waiters f)])
        (async-future-waiters-set! f '())
        (set-box! (async-future-state f) (cons 'done payload))
        (for-each (lambda (w) ((cdr w) payload)) waiters)))))





;;; ---------------------------------------------------------------- channels


(define async-waiter-dead?
  (lambda (ss) (not (eq? (unbox ss) 'waiting))))

(define async-channel-prune!
  (lambda (ch)
    ;; bounded pruning: drop dead waiters while scanning
    (async-channel-gets-set! ch
      (filter (lambda (w) (not (async-waiter-dead? (car w)))) (async-channel-gets ch)))
    (async-channel-puts-set! ch
      (filter (lambda (w) (not (async-waiter-dead? (cadr w)))) (async-channel-puts ch)))))

;;; Deliver value to the first live getter; returns #t on rendezvous.
(define async-channel-deliver-to-getter!
  (lambda (ch v)
    (let loop ([gs (async-channel-gets ch)])
      (cond
        [(null? gs) (async-channel-gets-set! ch '()) #f]
        [(async-waiter-dead? (caar gs)) (loop (cdr gs))]
        [((cdar gs) (cons 'values (list v)))
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
  (lambda (sched name parent-group migratable? entry)
    (let ([id (async-scheduler-next-id sched)])
      (async-scheduler-next-id-set! sched (fx+ 1 id))
      (make-async-task id name 'ready entry #f sched migratable?
        (async-snapshot-dynamic-state)
        (current-exception-state)
        parent-group #f '() #f '() #f #f #f #f #f #f #f #f (make-async-mutex)))))

(define ensure-child-group
  (lambda (task)
    (or (async-task-child-group task)
        (let ([grp (make-async-group (async-task-parent-group task))])
          (async-task-child-group-set! task grp)
          grp))))


;;; The body wrapper: run the thunk, then drain the child group.
(define run-task-entry
  (lambda (sched thunk)
    (let ([outcome
            (guard (c [else (cons 'failed c)])
              (call-with-values thunk (lambda vals (cons 'done vals))))])
      (let ([grp (async-task-child-group (async-scheduler-current-task sched))])
        (if grp
            (drain-task-group sched grp outcome)
            outcome)))))

;;; Wait for children; report unobserved failures by failing the owning task.
;;; The drain is shielded: the task's own cancellation does not interrupt the
;;; wait for canceled children's cleanup.
(define drain-task-group
  (lambda (sched grp outcome)
    (when (eq? (car outcome) 'failed)
      (group-cancel-children! grp #f))
    (let ([task (async-scheduler-current-task sched)])
      (async-task-cancel-shield?-set! task #t)
      (let loop ()
        (cond
          [(null? (async-task-group-children grp))
           (async-task-cancel-shield?-set! task #f)
           (cond
             [(and (eq? (car outcome) 'done)
                   (task-cancel-requested? task))
              (cons 'failed (task-cancellation-condition task))]
             [(eq? (car outcome) 'done)
              (let find ([us (async-task-group-unobserved grp)])
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
    (async-task-state-set! task state)
    (async-task-current-wait-set! task #f)
    (async-task-resumption-set! task #f)
    (case state
      [(completed) (async-task-result-values-set! task (cdr outcome))]
      [else (async-task-failure-condition-set! task (cdr outcome))])
    (sched-registry-remove! sched task)
    (let ([join-payload
            (case state
              [(completed) (cons 'values (cdr outcome))]
              [else (cons 'raise (cdr outcome))])])
      (with-async-mutex (async-task-mutex task)
        (when (and (eq? state 'failed) (pair? (async-task-join-waiters task)))
          (async-task-observed?-set! task #t))
        (for-each (lambda (w) ((cdr w) join-payload)) (async-task-join-waiters task))
        (async-task-join-waiters-set! task '())))
    (when (async-task-parent-group task)
      (group-child-terminated! (async-task-parent-group task) task))
    (void)))

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
        (and (task-terminal? task)
             (begin
               (when (eq? (async-task-state task) 'failed)
                 (async-task-observed?-set! task #t))
               (task-join-payload task))))
      (lambda (ss deliver)
        (if (task-terminal? task)
            (begin
              (when (eq? (async-task-state task) 'failed)
                (async-task-observed?-set! task #t))
              (deliver (task-join-payload task))
              #f)
            (begin
              (with-async-mutex (async-task-mutex task)
                (async-task-join-waiters-set! task
                  (cons (cons ss deliver) (async-task-join-waiters task))))
              (list 'join (async-task-id task)))))
      (lambda (vals) vals)
      (lambda (ss) (void)))))


;;; ------------------------------------------------------------- cancellation


;;; ------------------------------------------------------------------- yield


;;; --------------------------------------------------------- async dynamic-wind


;;; --------------------------------------------------------------- scheduler

(define async-make-scheduler
  (lambda (virtual?)
    (make-async-scheduler%
      (control:make-continuation-prompt-tag 'async-scheduler)
      (make-async-queue) (make-async-queue)
      (make-async-queue) (make-async-mutex)
      (if-feature pthreads (make-condition) #f)
      (make-eq-hashtable) 0 0 #f 'created virtual? 0 '() #f #f #f #f
      0 0 0 0 #f #f #f)))

(define async-drain-remote!
  (lambda (sched)
    (with-async-mutex (async-scheduler-remote-mutex sched)
      (let loop ()
        (unless (async-queue-empty? (async-scheduler-remote-queue sched))
          (let ([task (async-queue-pop! (async-scheduler-remote-queue sched))])
            (when (eq? (async-task-state task) 'ready)
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
       => (lambda (poll) (poll sched #t))]
      [else
       (if-feature pthreads
         (let ([ts (async-scheduler-timers sched)])
           (with-mutex (async-scheduler-remote-mutex sched)
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
                                   timeout)))))
         (let ([ts (async-scheduler-timers sched)])
           (unless (null? ts)
             (let* ([deadline (async-timer-deadline (car ts))]
                    [delta (max 0 (fx- deadline (async-monotonic-us)))])
               (sleep (make-time 'time-duration
                        (* (remainder delta 1000000) 1000)
                        (quotient delta 1000000)))))))])))

(define async-run-task-once
  (lambda (sched task)
    (async-scheduler-exec-count-set! sched (fx+ 1 (async-scheduler-exec-count sched)))
    (async-task-current-wait-set! task #f)
    (if (and (async-task-entry task) (task-cancel-requested? task))
        ;; a ready task observes cancellation before running user code
        (terminate-task! sched task 'canceled
          (cons 'raise (task-cancellation-condition task)))
        (begin
          (async-task-state-set! task 'running)
          (async-scheduler-current-task-set! sched task)
          (install-task-dynamic-state! sched task)
          (let ([outcome
                  (guard (c [else (cons 'internal-escape c)])
                    (if (async-task-entry task)
                        (let ([entry (async-task-entry task)])
                          (async-task-entry-set! task #f)
                          ($control-reset-at (async-scheduler-prompt-tag sched) #t
                            entry))
                        (let ([resumption (async-task-resumption task)]
                              [payload (async-task-payload task)])
                          (async-task-payload-set! task #f)
                          ;; rewinding captured dynamic-winds is part of the
                          ;; scheduling switch, not a user-level wind entry
                          (set-sched-switch! sched #t)
                          (resumption payload))))])
            (set-sched-switch! sched #f)
            (async-scheduler-current-task-set! sched #f)
            (save-task-dynamic-state! sched task)
            (cond
              [(eq? outcome async-suspend-token) (void)]
              [(and (pair? outcome) (eq? (car outcome) 'done))
               ;; a task that observes cancellation and still returns has
               ;; handled the request cooperatively
               (terminate-task! sched task 'completed outcome)]
              [(and (pair? outcome) (eq? (car outcome) 'failed))
               (if (and (task-cancel-requested? task)
                        ($async-cancellation-condition? (cdr outcome)))
                   (terminate-task! sched task 'canceled outcome)
                   (terminate-task! sched task 'failed outcome))]
              [else
               (terminate-task! sched task 'failed
                 (cons 'raise (cdr outcome)))]))))))

(define async-scheduler-run
  (lambda (sched)
    (let ([current (async-scheduler-current-queue sched)]
          [next (async-scheduler-next-queue sched)])
      (let loop ()
        (async-drain-remote! sched)
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
            (if (fx= ($async-scheduler-task-count sched) 0)
                (void)      ; all tasks terminal: scheduler done
                (begin
                  (async-idle-wait sched)
                  (loop)))
            (begin
              ($async-scheduler-turn-count-set! sched (fx+ 1 ($async-scheduler-turn-count sched)))
              (let turn-loop ()
                (unless (async-queue-empty? current)
                  (let ([task (async-queue-pop! current)])
                    (when (eq? (async-task-state task) 'ready)
                      (async-run-task-once sched task)))
                  (turn-loop)))
              (loop)))))))


;;; -------------------------------------------------------------- observability








;;; ------------------------------------------------------ public exports

(set! async-cancellation-condition? $async-cancellation-condition?)

(set! async-cancellation-reason $async-cancellation-reason)

(set! make-async-cancellation-condition
  (case-lambda
    [() ($make-async-cancellation-condition #f)]
    [(reason) ($make-async-cancellation-condition reason)]))

(record-writer (type-descriptor async-task)
  (lambda (r p wr)
    (fprintf p "#<task ~a~a ~a>"
      (task-id r)
      (if (task-name r) (format " ~s" (task-name r)) "")
      (task-state r))))

(record-writer (type-descriptor async-scheduler)
  (lambda (r p wr)
    (fprintf p "#<async-scheduler ~a tasks=~a turns=~a>"
      (async-scheduler-status r)
      ($async-scheduler-task-count r)
      ($async-scheduler-turn-count r))))

(record-writer (type-descriptor async-channel)
  (lambda (r p wr)
    (fprintf p "#<channel capacity=~a>" (async-channel-capacity r))))

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

(set-who! make-task-group
  (lambda ()
    (make-async-group #f)))

(set-who! task-group-wait
  (lambda (grp)
    (unless (task-group? grp) ($oops who "~s is not a task group" grp))
    (perform-operation (group-empty-operation grp))
    (let loop ()
      (with-async-mutex (async-task-group-mutex grp)
        (when (pair? (async-task-group-unobserved grp))
          (let ([u (car (async-task-group-unobserved grp))])
            (async-task-group-unobserved-set! grp (cdr (async-task-group-unobserved grp)))
            (unless (async-task-observed? (car u))
              (raise (cdr u)))))))
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
      (make-async-operation
        (lambda (ss)
          (let ([n (vector-length ops)])
            (if (fx= n 0)
                #f
                (let ([s (unbox start)])
                  (set-box! start (fxmod (fx+ s 1) n))
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
        (let ([ss (make-async-sync-state)])
          (let ([r ((operation-try op) ss)])
            (if r
                (async-deliver-operation-payload op r)
                (async-deliver-operation-payload op
                  ($async-suspend sched task ss
                    (lambda (ss*)
                      (async-task-nack-thunk-set! task
                        (lambda () ((operation-nack op) ss)))
                      ((operation-block op) ss
                        ($async-make-deliver ss task))))))))))))

(set-who! sleep-operation
  (lambda (seconds)
    (unless (async-valid-seconds? seconds)
      ($oops who "~s is not a nonnegative real number" seconds))
    (make-async-operation
      (lambda (ss)
        (and (<= seconds 0) (cons 'values '())))
      (lambda (ss deliver)
        (let* ([sched ($async-scheduler)]
               [deadline (+ (sched-now sched) (async-seconds->us seconds))])
          (async-schedule-timer! sched deadline deliver)
          (list 'sleep deadline)))
      (lambda (vals) vals)
      (lambda (ss) (void)))))

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
      (lambda (ss) (void)))))

(set-who! future-get
  (lambda (f)
    (perform-operation (future-operation f))))

(set-who! make-channel
  (case-lambda
    [() (make-async-channel% 0 (make-async-mutex) #f 0 0 '() '())]
    [(capacity)
     (unless (and (fixnum? capacity) (fx>= capacity 0))
       ($oops who "~s is not a nonnegative fixnum" capacity))
     (make-async-channel% capacity (make-async-mutex)
       (and (fx> capacity 0) (make-vector capacity)) 0 0 '() '())]))

(set-who! channel-put-operation
  (lambda (ch v)
    (unless (channel? ch) ($oops who "~s is not a channel" ch))
    (make-async-operation
      (lambda (ss)
        (with-async-mutex (async-channel-mutex ch)
          (async-channel-prune! ch)
          (if (async-channel-deliver-to-getter! ch v)
              (cons 'values '())
              (if (and (fx> (async-channel-capacity ch) 0)
                       (fx< (async-channel-bcount ch) (async-channel-capacity ch)))
                  (begin (async-buffer-push! ch v) (cons 'values '()))
                  #f))))
      (lambda (ss deliver)
        (with-async-mutex (async-channel-mutex ch)
          (async-channel-prune! ch)
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
                    (list 'channel-put ch))))))
      (lambda (vals) vals)
      (lambda (ss) (void)))))

(set-who! channel-get-operation
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
               (cons 'values (list v)))]
            [else
             (let ([got (box #f)])
               (if (async-channel-take-from-putter! ch
                     (lambda (pv deliver)
                       (if (deliver (cons 'values '()))
                           (begin (set-box! got pv) #t)
                           #f)))
                   (cons 'values (list (unbox got)))
                   #f))])))
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
                 (deliver (cons 'values (list v)))
                 #f)]
              [(async-channel-take-from-putter! ch
                 (lambda (pv pdeliver)
                   (if (pdeliver (cons 'values '()))
                       (begin (set-box! got pv) #t)
                       #f)))
               (deliver (cons 'values (list (unbox got))))
               #f]
              [else
               (async-channel-gets-set! ch
                 (append (async-channel-gets ch) (list (cons ss deliver))))
               (list 'channel-get ch)]))))
      (lambda (vals) vals)
      (lambda (ss) (void)))))

(set-who! channel-put
  (lambda (ch v)
    (perform-operation (channel-put-operation ch v))
    (void)))

(set-who! channel-get
  (lambda (ch)
    (perform-operation (channel-get-operation ch))))

(set-who! spawn-task
  (lambda (thunk . options)
    (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
    (let ([name #f] [group #f] [migratable? #f])
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
                 [task (async-make-task sched name grp migratable?
                         (lambda () (run-task-entry sched thunk)))])
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
            (async-queue-push! (async-scheduler-next-queue sched) task)
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
     (unless (or (task-terminal? task) (async-task-cancel-state task))
       (async-task-cancel-state-set! task 'requested)
       (async-task-cancel-condition-set! task (make-async-cancellation-condition reason))
       (when (async-task-child-group task)
         (group-cancel-children! (async-task-child-group task) reason))
       (when (and (eq? (async-task-state task) 'waiting)
                  (not (async-task-cancel-shield? task)))
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
    (let ([clock 'real] [parallelism 1])
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
              [else ($oops who "unrecognized run-async option ~s" k)]))
          (loop (cddr opts))))
      (unless (fx= parallelism 1)
        ($oops who "parallel scheduler groups are not available in this build"))
      (let* ([sched (async-make-scheduler (eq? clock 'virtual))]
             [root-group (make-async-group #f)]
             [root (async-make-task sched #f #f #f
                     (lambda () (run-task-entry sched thunk)))])
        (async-task-child-group-set! root root-group)
        (sched-registry-add! sched root)
        (async-queue-push! (async-scheduler-next-queue sched) root)
        (let ([old-sched ($async-scheduler)])
          (dynamic-wind
            (lambda ()
              ($async-scheduler sched)
              (async-scheduler-status-set! sched 'running)
              (async-scheduler-owner-thread-set! sched (get-thread-id)))
            (lambda ()
              (async-scheduler-run sched))
            (lambda ()
              ($async-io-shutdown sched)
              (async-scheduler-status-set! sched 'shutdown)
              (async-scheduler-owner-thread-set! sched #f)
              ($async-scheduler old-sched))))
        (case (async-task-state root)
          [(completed) (apply values (async-task-result-values root))]
          [(failed) (raise (async-task-failure-condition root))]
          [(canceled) (raise (async-task-failure-condition root))]
          [else ($oops who "scheduler stopped with root task ~s"
                  (async-task-state root))])))))

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
    ($async-scheduler-task-count sched)))

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

(set! $async-io-shutdown (lambda (sched) (void)))

(set! $async-scheduler-io-state
  (lambda (sched) (async-scheduler-io-state sched)))

(set! $async-scheduler-io-state-set!
  (lambda (sched v) (async-scheduler-io-state-set! sched v)))

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
