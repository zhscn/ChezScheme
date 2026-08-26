;;; ----------------------------------------------------------- utilities


(define async-debug-invariants?
  (let ([v (getenv "CHEZ_ASYNC_CHECK_INVARIANTS")])
    (and v (not (member v '("" "0" "false" "no"))))))

;;; The invariant self-test exercises rank ordering and recursive-acquisition
;;; detection without adding metadata or ancestry tracking to ordinary locks.
(define async-debug-lock-rank-active? #f)
(define async-debug-lock-metadata (box '()))

(define async-debug-register-lock!
  (lambda (lock rank name)
    (let loop ()
      (let ([state (async-atomic-box-ref async-debug-lock-metadata)])
        (unless (box-cas! async-debug-lock-metadata state
                  (cons (list lock rank name) state))
          (loop))))))

(define make-async-os-mutex
  (lambda () (if-feature pthreads (make-mutex) #f)))

(define make-async-debug-ranked-mutex
  (lambda (rank name)
    (let ([lock (make-async-os-mutex)])
      (async-debug-register-lock! lock rank name)
      lock)))

(define async-debug-lock-info
  (lambda (lock)
    (let loop ([state (async-atomic-box-ref async-debug-lock-metadata)])
      (cond
        [(null? state) #f]
        [(eq? (caar state) lock) (car state)]
        [else (loop (cdr state))]))))

(define async-debug-lock-state (box '()))

(define async-debug-lock-state-without
  (lambda (state id)
    (let loop ([state state])
      (cond
        [(null? state) '()]
        [(eqv? (caar state) id) (cdr state)]
        [else (cons (car state) (loop (cdr state)))]))))

(define async-debug-lock-push!
  (lambda (lock)
    (let ([id (get-thread-id)] [info (async-debug-lock-info lock)])
      (unless info
        ($oops 'with-async-mutex "unregistered ranked async mutex ~s" lock))
      (let loop ()
        (let* ([state (async-atomic-box-ref async-debug-lock-state)]
               [entry (assv id state)]
               [held (if entry (cdr entry) '())])
          (when (memq lock held)
            ($oops 'with-async-mutex "recursive async mutex acquisition: ~s"
              (caddr info)))
          (when (pair? held)
            (let* ([outer (car held)]
                   [outer-info (async-debug-lock-info outer)])
              (when (fx< (cadr info) (cadr outer-info))
                ($oops 'with-async-mutex
                  "async lock rank inversion: ~s (~s) after ~s (~s)"
                  (caddr info) (cadr info)
                  (caddr outer-info) (cadr outer-info)))))
          (unless (box-cas! async-debug-lock-state state
                    (cons (cons id (cons lock held))
                      (async-debug-lock-state-without state id)))
            (loop)))))))

(define async-debug-lock-pop!
  (lambda (lock)
    (let ([id (get-thread-id)])
      (let loop ()
        (let* ([state (async-atomic-box-ref async-debug-lock-state)]
               [entry (assv id state)]
               [held (if entry (cdr entry) '())])
          (unless (and (pair? held) (eq? (car held) lock))
            ($oops 'with-async-mutex "async mutex release order is invalid: ~s"
              (caddr (async-debug-lock-info lock))))
          (let* ([rest (async-debug-lock-state-without state id)]
                 [new-state
                  (if (null? (cdr held))
                      rest
                      (cons (cons id (cdr held)) rest))])
            (unless (box-cas! async-debug-lock-state state new-state)
              (loop))))))))

(define-syntax with-async-mutex
  (lambda (x)
    (syntax-case x ()
      [(_ m e1 e2 ...)
       (if-feature pthreads
         #'(let ([lock m])
             (if (and async-debug-invariants?
                      async-debug-lock-rank-active?)
                 (if (async-debug-lock-info lock)
                     (critical-section
                       (dynamic-wind
                         (lambda () (async-debug-lock-push! lock))
                         (lambda ()
                           (with-mutex lock e1 e2 ...))
                         (lambda () (async-debug-lock-pop! lock))))
                     (critical-section (with-mutex lock e1 e2 ...)))
                 (critical-section
                   (with-mutex lock e1 e2 ...))))
         #'(begin e1 e2 ...))])))

(define async-debug-runnable-mutex
  (if-feature pthreads (make-mutex) #f))
(define async-debug-runnable (make-eq-hashtable))

(define-syntax with-async-debug-runnable-mutex
  (lambda (x)
    (syntax-case x ()
      [(_ e1 e2 ...)
       (if-feature pthreads
         #'(critical-section
             (with-mutex async-debug-runnable-mutex e1 e2 ...))
         #'(begin e1 e2 ...))])))

(define-syntax async-invariant
  (syntax-rules ()
    [(_ ok? message object)
     (when async-debug-invariants?
       (unless ok?
         (fprintf (console-error-port) "async invariant failed: ~a: ~s\n"
           message object)
         (flush-output-port (console-error-port))
         ($oops 'async-invariant "~a: ~s" message object)))]))

(define async-debug-queue-claim!
  (lambda (task location)
    (when async-debug-invariants?
      (with-async-debug-runnable-mutex
        (async-invariant
          (not (hashtable-ref async-debug-runnable task #f))
          "task is present in more than one runnable queue" task)
        (async-invariant (eq? (async-task-state task) 'ready)
          "queued task is not ready" task)
        (hashtable-set! async-debug-runnable task location)))))

(define async-debug-queue-release!
  (lambda (task)
    (when async-debug-invariants?
      (with-async-debug-runnable-mutex
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

(define async-work-deque-distance
  (lambda (bottom top) (fx- bottom top)))

(define async-work-deque-push/raw!
  (lambda (deque value)
    (let* ([bottom (async-atomic-box-ref (async-work-deque-bottom deque))]
           [top (async-atomic-box-ref (async-work-deque-top deque))]
           [ring0 (async-atomic-box-ref (async-work-deque-ring deque))]
           [distance (async-work-deque-distance bottom top)]
           [ring
            (if (fx>= distance (async-work-ring-mask ring0))
                (async-work-deque-grow! deque top bottom ring0)
                ring0)])
      (async-invariant (fx>= distance 0)
        "work-deque indices describe a negative occupancy" deque)
      (vector-set! (async-work-ring-slots ring)
        (fxand bottom (async-work-ring-mask ring)) value)
      (async-atomic-box-set! (async-work-deque-bottom deque)
        (fx+ bottom 1)))))

(define async-work-deque-pop/raw!
  (lambda (deque)
    (let* ([bottom0 (async-atomic-box-ref (async-work-deque-bottom deque))]
           [bottom (fx- bottom0 1)])
      (async-atomic-box-set! (async-work-deque-bottom deque) bottom)
      (let* ([top (async-atomic-box-ref (async-work-deque-top deque))]
             [distance (async-work-deque-distance bottom top)])
        (cond
          [(fx< distance 0)
           (async-atomic-box-set! (async-work-deque-bottom deque) top)
           #f]
          [else
           (let* ([ring (async-atomic-box-ref (async-work-deque-ring deque))]
                  [slot (fxand bottom (async-work-ring-mask ring))]
                  [value (vector-ref (async-work-ring-slots ring) slot)])
             (if (and (fx= distance 0)
                      (not (box-cas! (async-work-deque-top deque)
                             top (fx+ top 1))))
                 (begin
                   (async-atomic-box-set! (async-work-deque-bottom deque)
                     (fx+ top 1))
                   #f)
                 (begin
                   (vector-set! (async-work-ring-slots ring) slot #f)
                   (when (fx= distance 0)
                     (async-atomic-box-set! (async-work-deque-bottom deque)
                       (fx+ top 1)))
                   (unless value
                     ($oops 'async-work-deque-pop/raw!
                       "published work-deque slot is empty"))
                   value)))])))))

(define async-work-deque-steal/raw!
  (lambda (deque)
    (let* ([top (async-atomic-box-ref (async-work-deque-top deque))]
           [bottom (async-atomic-box-ref (async-work-deque-bottom deque))])
      (and (fx> (async-work-deque-distance bottom top) 0)
           (let* ([ring (async-atomic-box-ref (async-work-deque-ring deque))]
                  [slot (fxand top (async-work-ring-mask ring))]
                  [value (vector-ref (async-work-ring-slots ring) slot)])
             (and (box-cas! (async-work-deque-top deque) top
                    (fx+ top 1))
                  (begin
                    (unless value
                      ($oops 'async-work-deque-steal/raw!
                        "published work-deque slot is empty"))
                    (vector-set! (async-work-ring-slots ring) slot #f)
                    value)))))))

(define async-debug-work-deque-check!
  (lambda (deque expected)
    (let* ([top (async-atomic-box-ref (async-work-deque-top deque))]
           [bottom (async-atomic-box-ref (async-work-deque-bottom deque))]
           [ring (async-atomic-box-ref (async-work-deque-ring deque))]
           [slots (async-work-ring-slots ring)]
           [mask (async-work-ring-mask ring)])
      (unless (and (fx= (async-work-deque-distance bottom top)
                        (length expected))
                   (fx= mask (fx- (vector-length slots) 1))
                   (fx= (fxand (vector-length slots) mask) 0))
        ($oops 'async-work-deque-model-test
          "work-deque geometry disagrees with the reference model"))
      (let loop ([i top] [expected expected])
        (unless (null? expected)
          (unless (eqv? (vector-ref slots (fxand i mask)) (car expected))
            ($oops 'async-work-deque-model-test
              "work-deque contents disagree with the reference model"))
          (loop (fx+ i 1) (cdr expected)))))))

(define async-debug-work-deque-model-test
  (lambda (rounds)
    (unless (and (fixnum? rounds) (fx> rounds 0))
      ($oops 'async-work-deque-model-test "invalid round count ~s" rounds))
    (let ([deque (make-async-work-deque)]
          [expected '()]
          [next-value 0]
          [seed 324508639])
      (define (push!)
        (let ([value next-value])
          (set! next-value (fx+ next-value 1))
          (async-work-deque-push/raw! deque value)
          (set! expected (append expected (list value)))))
      (define (pop!)
        (let ([actual (async-work-deque-pop/raw! deque)])
          (if (null? expected)
              (unless (not actual)
                ($oops 'async-work-deque-model-test
                  "owner pop returned work from an empty deque"))
              (let ([wanted (car (reverse expected))])
                (unless (eqv? actual wanted)
                  ($oops 'async-work-deque-model-test
                    "owner pop disagrees with the reference model"))
                (set! expected (reverse (cdr (reverse expected))))))))
      (define (steal!)
        (let ([actual (async-work-deque-steal/raw! deque)])
          (if (null? expected)
              (unless (not actual)
                ($oops 'async-work-deque-model-test
                  "steal returned work from an empty deque"))
              (begin
                (unless (eqv? actual (car expected))
                  ($oops 'async-work-deque-model-test
                    "steal disagrees with the reference model"))
                (set! expected (cdr expected))))))
      (do ([i 0 (fx+ i 1)]) ((fx= i 96)) (push!))
      (async-debug-work-deque-check! deque expected)
      (do ([i 0 (fx+ i 1)]) ((fx= i rounds))
        (set! seed
          (fxand (fx+/wraparound (fx*/wraparound seed 1103515245) 12345)
                 (greatest-fixnum)))
        (case (fxmod seed 5)
          [(0 1) (push!)]
          [(2) (pop!)]
          [else (steal!)])
        (async-debug-work-deque-check! deque expected))
      (let drain ([owner? #t])
        (unless (null? expected)
          (if owner? (pop!) (steal!))
          (async-debug-work-deque-check! deque expected)
          (drain (not owner?))))
      (pop!)
      (steal!)
      (async-debug-work-deque-check! deque '())
      #t)))

(define async-debug-work-deque-contention-test
  (lambda (count thief-count)
    (unless (and (fixnum? count) (fx> count 0)
                 (fixnum? thief-count) (fx> thief-count 0))
      ($oops 'async-work-deque-contention-test
        "invalid contention dimensions ~s ~s" count thief-count))
    (if-feature pthreads
      (let ([deque (make-async-work-deque)]
            [seen (make-bytevector count 0)]
            [seen-count 0]
            [seen-mutex (make-mutex)]
            [pushing? (box #t)]
            [failure (box #f)])
        (define (record! value)
          (with-mutex seen-mutex
            (if (or (not (fixnum? value)) (fx< value 0) (fx>= value count))
                (set-box! failure (list 'invalid value))
                (let ([n (bytevector-u8-ref seen value)])
                  (if (fx> n 0)
                      (set-box! failure (list 'duplicate value))
                      (begin
                        (bytevector-u8-set! seen value 1)
                        (set! seen-count (fx+ seen-count 1))))))))
        (define (thief)
          (let loop ()
            (let ([value (async-work-deque-steal/raw! deque)])
              (cond
                [value (record! value) (loop)]
                [(or (async-atomic-box-ref pushing?)
                     (fx> (async-work-deque-distance
                            (async-atomic-box-ref
                              (async-work-deque-bottom deque))
                            (async-atomic-box-ref
                              (async-work-deque-top deque)))
                          0))
                 (sleep (make-time 'time-duration 1000 0))
                 (loop)]))))
        (let ([threads
               (let loop ([i 0] [threads '()])
                 (if (fx= i thief-count)
                     threads
                     (loop (fx+ i 1) (cons (fork-thread thief) threads))))])
          (do ([i 0 (fx+ i 1)]) ((fx= i count))
            (async-work-deque-push/raw! deque i)
            (when (fx= (fxand i 7) 7)
              (let ([value (async-work-deque-pop/raw! deque)])
                (when value (record! value)))))
          (async-atomic-box-set! pushing? #f)
          (let drain ()
            (let ([value (async-work-deque-pop/raw! deque)])
              (when value (record! value) (drain))))
          (for-each thread-join threads)
          (unless (and (not (unbox failure)) (fx= seen-count count))
            ($oops 'async-work-deque-contention-test
              "work-deque contention lost or duplicated work: ~s/~s ~s"
              seen-count count (unbox failure)))
          #t))
      #t)))

(define async-debug-lock-rank-test
  (lambda ()
    (if (not async-debug-invariants?)
        'disabled
        (dynamic-wind
          (lambda () (set! async-debug-lock-rank-active? #t))
          (lambda ()
            (let ([low (make-async-debug-ranked-mutex 1 'rank-test-low)]
                  [high (make-async-debug-ranked-mutex 2 'rank-test-high)])
              (let ([ordered?
                     (with-async-mutex low
                       (with-async-mutex high #t))]
                    [inversion?
                     (guard (condition [else #t])
                       (with-async-mutex high
                         (with-async-mutex low #f))
                       #f)]
                    [recursive?
                     (guard (condition [else #t])
                       (with-async-mutex low
                         (with-async-mutex low #f))
                       #f)])
                (and ordered? inversion? recursive?))))
          (lambda () (set! async-debug-lock-rank-active? #f))))))

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
(define async-channel-try-put! #f)
(define async-channel-try-receive! #f)

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
  (nongenerative async-scheduler-layer16)
  (sealed #t)
  (fields
    (mutable native-fiber)          ; worker's adopted scheduler fiber
    (immutable group $async-scheduler-group) ; owning scheduler group
    (immutable group-index)         ; stable index within the group
    (immutable current-queue)       ; tasks run this turn
    (immutable next-queue)          ; tasks run next turn
    (immutable work-deque)          ; owner-bottom/thief-top Chase--Lev deque
    (immutable remote-queue)        ; cross-thread submissions
    (immutable remote-mutex)
    (immutable remote-cond)
    (mutable idle-previous)         ; group idle-list links
    (mutable idle-next)
    (mutable idle-linked?)
    (mutable steal-seed)            ; owner-only victim permutation state
    (mutable owner-thread)          ; thread id running the loop, or #f
    (mutable status)                ; created | running | shutdown
    (mutable virtual?)              ; deterministic virtual clock
    (mutable vtime)                 ; virtual clock, microseconds
    (immutable timers)              ; indexed min-heap of async-timer
    (mutable current-task)          ; running task or #f
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
  (nongenerative async-scheduler-group-layer2)
  (sealed #t)
  (fields
    (immutable mutex)
    (immutable condition)
    (immutable ready-queue)         ; ready migratable tasks
    (immutable tasks)               ; debug-only stable id -> task registry
    (immutable task-count-box)
    (mutable schedulers)
    (mutable root-task)
    (immutable next-task-id-box)
    (mutable idle-schedulers)       ; intrusive list head
    (immutable idle-count-box)
    (immutable work-count-box)      ; published Chase--Lev entries
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
  (nongenerative async-task-layer14)
  (sealed #t)
  (fields
    (immutable id)
    (immutable name)
    (mutable state)                 ; ready running waiting completed failed canceled
    (mutable entry)                 ; thunk before first run, #f after
    (mutable native-fiber)          ; VM-owned task execution context
    (immutable scheduler-group)     ; scheduler group in which the task runs
    (mutable scheduler)             ; current or most recent execution scheduler
    (mutable wait-scheduler)        ; scheduler that owns the current wait
    (immutable migratable?)
    (mutable resume-pinned?)        ; next I/O resumption stays on wait owner
    (mutable suspension-state)      ; #f | unwinding | parking | parked | delivered
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
    (mutable owned-mutexes)         ; short list or eq-hashtable of owned locks
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
    (mutable attached?)             ; linked in parent's active subgroup list
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

(define async-task-observe-failure!
  (lambda (task)
    (with-async-mutex (async-task-mutex task)
      (if (async-task-observed? task)
          #f
          (begin
            (async-task-observed?-set! task #t)
            #t)))))

(define async-debug-check-native-fiber!
  (lambda (who fiber expected-states scheduler?)
    (when async-debug-invariants?
      (async-invariant ($native-fiber? fiber)
        "async object retains a malformed native fiber" who)
      (let ([state ($native-fiber-state fiber)]
            [owner ($native-fiber-owner fiber)]
            [flags ($native-fiber-flags fiber)])
        (async-invariant (memq state expected-states)
          "native-fiber lifecycle disagrees with async state"
          (list who state expected-states))
        (async-invariant
          (eq? scheduler?
            (not (fx= (fxlogand flags
                         (constant native-fiber-flag-scheduler))
                       0)))
          "native-fiber role disagrees with async ownership" who)
        ;; A migratable task can be claimed and resumed by another worker as
        ;; soon as this worker publishes its suspension.  Relate `running` to
        ;; the current VM root only for fibers owned by this worker.
        (when (and owner (fx= owner (get-thread-id)))
          (async-invariant
            (eq? (eq? state 'running) (eq? fiber ($current-native-fiber)))
            "native-fiber running root disagrees with the current VM root"
            who))))))

;;; Cancellation points raise whenever cancellation is in effect, unless the
;;; current wait is shielded (internal group draining).
(define async-check-cancellation!
  (lambda (task)
    (when (and (task-cancel-requested? task)
               (not (async-task-cancel-shield? task)))
      (raise (task-cancellation-condition task)))))

(define async-check-operation-entry!
  (lambda (task)
    (async-check-cancellation! task)
    (let ([context (and (not (async-task-cancel-shield? task))
                        (async-current-context))])
      (when (and context (async-context-canceled?/raw context))
        (raise (async-context-cancellation-condition context)))
      context)))

(define async-return-payload
  (lambda (payload)
    (if (eq? (car payload) 'values)
        (apply values (cdr payload))
        (raise (cdr payload)))))

(define async-task-group-runnable?
  (lambda (task)
    (and (async-task-migratable? task)
         (async-group-parallel? (async-task-scheduler-group task)))))

(define sched-now
  (lambda (sched)
    (if (async-scheduler-virtual? sched)
        (async-scheduler-vtime sched)
        (async-monotonic-us))))

(define async-scheduler-group-task-count
  (lambda (group)
    (async-atomic-box-ref (async-scheduler-group-task-count-box group))))

(define async-next-task-id!
  (lambda (group)
    (let ([id-box (async-scheduler-group-next-task-id-box group)])
      (let loop ([id (async-atomic-box-ref id-box)])
        (let ([next (if (fx= id (most-positive-fixnum)) 0 (fx+ id 1))])
          (if (box-cas! id-box id next)
              id
              (loop (async-atomic-box-ref id-box))))))))

(define sched-registry-add/raw!
  (lambda (sched task)
    (let* ([group ($async-scheduler-group sched)]
           [tasks (async-scheduler-group-tasks group)]
           [id (async-task-id task)])
      (unless (hashtable-ref tasks id #f)
        (hashtable-set! tasks id task)
        (async-atomic-box-add! (async-scheduler-group-task-count-box group) 1))
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
        (async-atomic-box-add! (async-scheduler-group-task-count-box group) -1))
      (async-invariant
        (fx= (async-scheduler-group-task-count group)
             (hashtable-size tasks))
        "task registry count does not match registry size" group))))

(define sched-registry-add!
  (lambda (sched task)
    (let ([group ($async-scheduler-group sched)])
      (if async-debug-invariants?
          (with-async-mutex (async-scheduler-group-mutex group)
            (sched-registry-add/raw! sched task))
          (async-atomic-box-add! (async-scheduler-group-task-count-box group) 1)))))

(define sched-registry-remove!
  (lambda (sched task)
    (let ([group ($async-scheduler-group sched)])
      (if async-debug-invariants?
          (with-async-mutex (async-scheduler-group-mutex group)
            (sched-registry-remove/raw! sched task))
          (async-atomic-box-add! (async-scheduler-group-task-count-box group) -1)))))

(define async-group-parallel?
  (lambda (group)
    (fx> (vector-length (async-scheduler-group-schedulers group)) 1)))

(define async-group-has-work?
  (lambda (group)
    (fx> (async-atomic-box-ref
           (async-scheduler-group-work-count-box group)) 0)))

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
      (with-mutex (async-scheduler-group-mutex group)
        (async-invariant
          (and (fx= (async-scheduler-group-task-count group) 0)
               (fx= (hashtable-size (async-scheduler-group-tasks group)) 0))
          "scheduler group retained terminal tasks" group)
        (async-invariant (not (async-group-has-work? group))
          "scheduler group retained stealable work" group))
      (with-async-debug-runnable-mutex
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
      (if (not idle)
          #f
          (begin
            (let ([next (async-scheduler-idle-next idle)])
              (async-scheduler-group-idle-schedulers-set! group next)
              (when next (async-scheduler-idle-previous-set! next #f))
              (async-scheduler-idle-previous-set! idle #f)
              (async-scheduler-idle-next-set! idle #f)
              (async-scheduler-idle-linked?-set! idle #f)
              (async-atomic-box-add!
                (async-scheduler-group-idle-count-box group) -1)
              idle))))))

(define async-group-next-wake-target!
  (lambda (group)
    (and (fx> (async-atomic-box-ref
                (async-scheduler-group-idle-count-box group)) 0)
         (with-async-mutex (async-scheduler-group-mutex group)
           (async-group-take-idle/raw! group)))))

(define async-group-mark-idle!
  (lambda (sched)
    (let ([group ($async-scheduler-group sched)])
      (with-async-mutex (async-scheduler-group-mutex group)
        (unless (async-scheduler-idle-linked? sched)
          (let ([head (async-scheduler-group-idle-schedulers group)])
            (async-scheduler-idle-previous-set! sched #f)
            (async-scheduler-idle-next-set! sched head)
            (when head (async-scheduler-idle-previous-set! head sched))
            (async-scheduler-group-idle-schedulers-set! group sched)
            (async-scheduler-idle-linked?-set! sched #t)
            (async-atomic-box-add!
              (async-scheduler-group-idle-count-box group) 1)))))))

(define async-group-unmark-idle!
  (lambda (sched)
    (let ([group ($async-scheduler-group sched)])
      (with-async-mutex (async-scheduler-group-mutex group)
        (when (async-scheduler-idle-linked? sched)
          (let ([previous (async-scheduler-idle-previous sched)]
                [next (async-scheduler-idle-next sched)])
            (if previous
                (async-scheduler-idle-next-set! previous next)
                (async-scheduler-group-idle-schedulers-set! group next))
            (when next (async-scheduler-idle-previous-set! next previous))
            (async-scheduler-idle-previous-set! sched #f)
            (async-scheduler-idle-next-set! sched #f)
            (async-scheduler-idle-linked?-set! sched #f)
            (async-atomic-box-add!
              (async-scheduler-group-idle-count-box group) -1)))))))

(define async-work-push!
  (lambda (sched task)
    (async-debug-check-owner! sched)
    (async-debug-queue-claim! task 'work)
    (async-work-deque-push/raw! (async-scheduler-work-deque sched) task)
    (async-atomic-box-add!
      (async-scheduler-group-work-count-box
        ($async-scheduler-group sched)) 1)))

(define async-work-pop!
  (lambda (sched)
    (async-debug-check-owner! sched)
    (let ([task
           (async-work-deque-pop/raw!
             (async-scheduler-work-deque sched))])
      (when task
        (async-debug-queue-release! task)
        (async-atomic-box-add!
          (async-scheduler-group-work-count-box
            ($async-scheduler-group sched)) -1))
      task)))

(define async-work-steal-one!
  (lambda (victim)
    (let ([task
           (async-work-deque-steal/raw!
             (async-scheduler-work-deque victim))])
      (when task
        (async-debug-queue-release! task)
        (async-atomic-box-add!
          (async-scheduler-group-work-count-box
            ($async-scheduler-group victim)) -1))
      task)))

;;; Repeated single-item claims preserve the Chase--Lev last-item race while
;;; still amortizing victim selection and wakeup costs across a batch.
(define async-work-steal-batch!
  (lambda (victim)
    (let ([first (async-work-steal-one! victim)])
      (and first
           (let* ([deque (async-scheduler-work-deque victim)]
                  [available
                   (async-work-deque-distance
                     (async-atomic-box-ref (async-work-deque-bottom deque))
                     (async-atomic-box-ref (async-work-deque-top deque)))]
                  [extra (if (fx> available 1)
                             (fxquotient available 2)
                             available)])
             (let loop ([remaining extra] [tasks (list first)])
               (if (fx= remaining 0)
                   (reverse tasks)
                   (let ([task (async-work-steal-one! victim)])
                     (if task
                         (loop (fx- remaining 1) (cons task tasks))
                         (reverse tasks))))))))))

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
                       [self (async-scheduler-group-index sched)]
                       [start (fxmod (fx+ (async-scheduler-steal-seed sched) 1) n)]
                       [budget (let ([others (fx- n 1)])
                                 (if (fx< others 8) others 8))])
                  (async-scheduler-steal-seed-set! sched start)
                  (let loop ([index start] [remaining budget])
                    (if (fx= remaining 0)
                        #f
                        (if (fx= index self)
                            (loop (fxmod (fx+ index 1) n) remaining)
                            (let ([batch
                                   (async-work-steal-batch!
                                     (vector-ref schedulers index))])
                              (if batch
                                  (begin
                                    ;; Keep one task for immediate execution
                                    ;; and amortize the successful steal by
                                    ;; publishing the remainder locally.
                                    (for-each
                                      (lambda (task)
                                        (async-work-push! sched
                                          (async-adopt-work! sched task)))
                                      (cdr batch))
                                    (async-adopt-work! sched
                                      (car batch)))
                                  (loop (fxmod (fx+ index 1) n)
                                    (fx- remaining 1))))))))))))))

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
      (with-async-mutex (async-scheduler-group-mutex group)
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
             (not (async-task-resume-pinned? task)))
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

;;; Mark the suspension as leaving the task runner. Publication is deferred
;;; until async-run-task-once has returned to the scheduler loop and the VM
;;; has committed the task fiber's parked state.
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
      (make-async-wait-queue) '() parent #f context
      (make-async-os-mutex))))

;; The group mutex protects both the direct-child census and the active
;; subgroup links.  Empty groups are detached from their parent so that a
;; long-lived task does not retain every explicit group it has used.
(define async-group-empty?/locked
  (lambda (grp)
    (and (fx= (async-task-group-child-count grp) 0)
         (null? (async-task-group-subgroups grp)))))

(define async-group-check-locked!
  (lambda (grp)
    (when async-debug-invariants?
      (async-invariant
        (fx= (async-task-group-child-count grp)
             (hashtable-size (async-task-group-children grp)))
        "task-group child count disagrees with its registry" grp)
      (let loop ([subgroups (async-task-group-subgroups grp)] [seen '()])
        (unless (null? subgroups)
          (let ([subgroup (car subgroups)])
            (async-invariant ($async-task-group? subgroup)
              "task-group contains a malformed subgroup" grp)
            (async-invariant (not (memq subgroup seen))
              "task-group contains a duplicate subgroup" grp)
            (loop (cdr subgroups) (cons subgroup seen))))))))

(define async-group-notify-empty!
  (lambda (grp)
    (let-values ([(waiters parent parent-empty?)
                  (with-async-mutex (async-task-group-mutex grp)
                    (async-group-check-locked! grp)
                    (if (async-group-empty?/locked grp)
                        (let ([waiters
                               (async-wait-queue-drain!
                                 (async-task-group-waiters grp))]
                              [parent (async-task-group-parent grp)])
                          (if (and parent (async-task-group-attached? grp))
                              (with-async-mutex (async-task-group-mutex parent)
                                (async-task-group-subgroups-set! parent
                                  (remq grp
                                    (async-task-group-subgroups parent)))
                                (async-task-group-attached?-set! grp #f)
                                (async-group-check-locked! parent)
                                (values waiters parent
                                  (async-group-empty?/locked parent)))
                              (values waiters #f #f)))
                        (values '() #f #f)))])
      (for-each (lambda (w) ((cdr w) (cons 'values '()))) waiters)
      (when parent-empty?
        (async-group-notify-empty! parent)))))

(define async-group-add-child!
  (lambda (grp task parent)
    (with-async-mutex (async-task-group-mutex grp)
      ;; The first explicit use fixes the structural parent.  Reuse remains
      ;; valid within that scope.  The captured cancellation context and
      ;; parent link make a group specific to that structured lifetime.
      (when parent
        (let ([existing (async-task-group-parent grp)])
          (when (and existing
                     (not (eq? existing parent))
                     (not (async-task-group-attached? grp)))
            ($oops 'spawn-task
              "task group cannot be reused from another completed scope"))
          (unless existing
            (async-task-group-parent-set! grp parent))
          (set! parent (async-task-group-parent grp))
          (unless (async-task-group-attached? grp)
            (with-async-mutex (async-task-group-mutex parent)
              (unless (memq grp (async-task-group-subgroups parent))
                (async-task-group-subgroups-set! parent
                  (cons grp (async-task-group-subgroups parent))))
              (async-task-group-attached?-set! grp #t)
              (async-group-check-locked! parent)))))
      (hashtable-set! (async-task-group-children grp) task task)
      (async-task-group-child-count-set! grp
        (fx+ 1 (async-task-group-child-count grp)))
      (async-group-check-locked! grp))))

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
    (with-async-mutex (async-task-group-mutex grp)
      (when (hashtable-ref (async-task-group-children grp) task #f)
        (hashtable-delete! (async-task-group-children grp) task)
        (async-task-group-child-count-set! grp
          (fx- (async-task-group-child-count grp) 1)))
      (when (eq? (async-task-state task) 'failed)
        (let ([failure
               (cons task (async-task-failure-condition task))])
          (async-task-group-unobserved-set! grp
            (cons failure (async-task-group-unobserved grp)))
          ;; An attached explicit group participates in its parent's failure
          ;; boundary as well as retaining the failure for task-group-wait.
          (let ([parent (and (async-task-group-attached? grp)
                             (async-task-group-parent grp))])
            (when parent
              (with-async-mutex (async-task-group-mutex parent)
                (async-task-group-unobserved-set! parent
                  (cons failure (async-task-group-unobserved parent))))))))
      (async-group-check-locked! grp))
    (async-group-notify-empty! grp)))

(define group-empty-operation
  (lambda (grp)
    (let ([token (list 'task-group-empty-operation)])
      (make-async-operation
      (lambda (ss)
        (with-async-mutex (async-task-group-mutex grp)
          (async-group-check-locked! grp)
          (and (async-group-empty?/locked grp)
               (cons 'values '()))))
      (lambda (ss deliver)
        (let ([blocked?
               (with-async-mutex (async-task-group-mutex grp)
                 (async-group-check-locked! grp)
                 (if (async-group-empty?/locked grp)
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
