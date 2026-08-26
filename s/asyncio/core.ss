;;; ------------------------------------------------------------ records

;;; Intrusive owner-locked queues are used for I/O waiters and serialized
;;; filesystem operations.  Cancellation unlinks the exact node in O(1).
(define-record-type (aio-queue-node make-aio-queue-node aio-queue-node?)
  (nongenerative)
  (sealed #t)
  (fields (immutable value) (mutable owner) (mutable previous) (mutable next)))

(define-record-type (aio-queue make-aio-queue% aio-queue?)
  (nongenerative)
  (sealed #t)
  (fields (mutable head) (mutable tail)))

(define make-aio-queue
  (lambda () (make-aio-queue% #f #f)))

(define aio-queue-empty?
  (lambda (queue) (not (aio-queue-head queue))))

(define aio-queue-push!
  (lambda (queue value)
    (let* ([tail (aio-queue-tail queue)]
           [node (make-aio-queue-node value queue tail #f)])
      (if tail
          (aio-queue-node-next-set! tail node)
          (aio-queue-head-set! queue node))
      (aio-queue-tail-set! queue node)
      node)))

(define aio-queue-remove!
  (lambda (queue node)
    (and (eq? (aio-queue-node-owner node) queue)
         (let ([previous (aio-queue-node-previous node)]
               [next (aio-queue-node-next node)])
           (if previous
               (aio-queue-node-next-set! previous next)
               (aio-queue-head-set! queue next))
           (if next
               (aio-queue-node-previous-set! next previous)
               (aio-queue-tail-set! queue previous))
           (aio-queue-node-owner-set! node #f)
           (aio-queue-node-previous-set! node #f)
           (aio-queue-node-next-set! node #f)
           #t))))

(define aio-queue-pop!
  (lambda (queue)
    (let ([node (aio-queue-head queue)])
      (and node
           (begin
             (aio-queue-remove! queue node)
             (aio-queue-node-value node))))))

(define aio-queue-peek
  (lambda (queue)
    (let ([node (aio-queue-head queue)])
      (and node (aio-queue-node-value node)))))

(define aio-queue-drain!
  (lambda (queue)
    (let loop ([values '()])
      (let ([value (aio-queue-pop! queue)])
        (if value (loop (cons value values)) (reverse values))))))

(define aio-queue-pop-live!
  (lambda (queue)
    (let loop ()
      (let ([waiter (aio-queue-pop! queue)])
        (and waiter
             (if (aio-waiter-dead? (car waiter)) (loop) waiter))))))

(define aio-queue-peek-live
  (lambda (queue)
    (let loop ()
      (let ([waiter (aio-queue-peek queue)])
        (and waiter
             (if (aio-waiter-dead? (car waiter))
                 (begin (aio-queue-pop! queue) (loop))
                 waiter))))))

;;; One per scheduler that has touched I/O.  Requests are in-flight native
;;; operations keyed by request id; handles are live listeners/streams keyed
;;; by handle id.  The tables are guarded because a cancellation nack can run
;;; on a thread other than the scheduler's.
(define-record-type (aio-state make-aio-state aio-state?)
  (nongenerative aio-state-layer4)
  (sealed #t)
  (fields
    (immutable owner)             ; scheduler that drives this loop
    (immutable loop)              ; void* aio_loop_t
    (immutable wakeup)            ; void* uv_async_t
    (immutable bridge)            ; void* uv_timer_t
    (mutable next-id)
    (immutable requests)          ; id -> aio-req
    (immutable requests-mutex)
    (immutable handles)           ; id -> weak-cons wrapper #t
    (immutable files)             ; fd -> weak-cons async-file #t
    (immutable directories)       ; native pointer -> weak-cons async-directory #t
    (immutable completion-buffer) ; scratch for one native completion
    (mutable commands)            ; owner-thread thunks, newest first
    (immutable command-mutex)     ; also guards closing and wakeup lifetime
    (mutable stop-set)            ; streams that may need uv_read_stop
    (immutable stop-mutex)
    (mutable poll-count)          ; owner-only guardian maintenance cadence
    (mutable closing?)
    (immutable guardian)
    (immutable file-guardian)
    (immutable directory-guardian)))

(define-record-type (aio-req make-aio-req aio-req?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable kind)              ; 'write 'connect 'shutdown 'fs 'dns
    (immutable handle)            ; owning stream/file, or #f
    (mutable deliver)             ; deliver closure or #f
    (immutable cancel-data)       ; ctx pointer for uv_cancel, or #f
    (immutable finish)            ; canceled? status aux -> payload or #f
    (mutable canceled?)))

;;; wrappers for listeners and streams; the id doubles as the native handle's
;;; data field and never changes, so a recycled descriptor cannot be confused
;;; with a stale wrapper
(define-record-type (aio-handle make-aio-handle aio-handle?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable id)
    (immutable handle)            ; void*
    (immutable kind)              ; 'tcp-stream 'pipe-stream 'tcp-listener 'pipe-listener
    (immutable state)             ; owning aio-state
    (immutable path)              ; descriptive string or #f
    (mutable port-owned?)
    (immutable mutex)
    (mutable closing?)
    (mutable closed?)             ; close callback has run
    (immutable read-queue)        ; aio-queue of (ss . deliver)
    (mutable reading?)
    (mutable eof?)
    (immutable accept-queue)      ; aio-queue of (ss . deliver)
    (mutable result)))            ; process exit result, or #f

(define-record-type (async-file make-async-file% %async-file?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable fd)
    (immutable path)
    (immutable state)
    (mutable port-owned?)
    (mutable offset)
    (mutable closed?)
    (immutable mutex)
    (mutable busy?)
    (immutable queue)))

(define-record-type (async-directory make-async-directory% %async-directory?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable pointer)
    (immutable path)
    (immutable state)
    (mutable closed?)
    (immutable mutex)
    (mutable busy?)
    (immutable queue)))

;;; --------------------------------------------------------- invariants

(define aio-debug-invariants?
  ($async-debug-invariants?))

(define-syntax aio-invariant
  (syntax-rules ()
    [(_ ok? message object)
     (when aio-debug-invariants?
       (unless ok?
         ($oops 'async-io-invariant "~a: ~s" message object)))]))

;;; ------------------------------------------------------------ helpers

(define aio-next-id
  (lambda (st)
    (aio-debug-check-owner! st)
    (let ([id (aio-state-next-id st)])
      (aio-state-next-id-set! st (fx+ id 1))
      id)))

(define aio-waiter-dead?
  (lambda (ss) (not ($async-sync-state-live? ss))))

;;; Deliver a single event to the oldest live waiter.  A cancellation can win
;;; after the waiter is unlinked, so retry until a delivery claims its sync
;;; state or the queue becomes empty.
(define aio-deliver-one-waiter!
  (lambda (h queue payload)
    (let loop ()
      (let ([waiter
             (with-aio-mutex (aio-handle-mutex h)
               (aio-queue-pop-live! queue))])
        (and waiter
             (or ((cdr waiter) payload) (loop)))))))

(define aio-io-condition
  (lambda (operation handle path code)
    (make-async-io-condition% operation handle path code)))

(define aio-closed-condition
  (lambda (operation handle)
    (make-async-io-condition% operation handle (aio-handle-path handle) 'closed)))

(define aio-debug-check-owner!
  (lambda (st)
    (when aio-debug-invariants?
      (let ([owner (aio-state-owner st)])
        (aio-invariant (eq? (current-async-scheduler) owner)
          "libuv loop operation ran under a foreign scheduler" st)
        (aio-invariant ($async-scheduler-owner-thread? owner)
          "libuv loop operation ran on a foreign thread" st)))))

(define aio-check-state-scope!
  (lambda (who st message)
    (let ([sched (current-async-scheduler)])
      (unless (and sched
                   (eq? ($async-scheduler-group-token sched)
                        ($async-scheduler-group-token (aio-state-owner st))))
        ($oops who message)))))

(define bv->cstring
  (lambda (bv)
    (let* ([n (bytevector-length bv)]
           [len (let loop ([i 0])
                  (if (or (fx= i n) (fx= (bytevector-u8-ref bv i) 0))
                      i
                      (loop (fx+ i 1))))]
           [s (make-string len)])
      (do ([i 0 (fx+ i 1)]) ((fx= i len) s)
        (string-set! s i (integer->char (bytevector-u8-ref bv i)))))))

(define aio-symbols->flag-bits
  (lambda (who symbols collection-name element-name mapping)
    (unless (and (list? symbols) (for-all symbol? symbols))
      ($oops who "~s is not a list of ~a" symbols collection-name))
    (fold-left
      (lambda (bits symbol)
        (let ([entry (assq symbol mapping)])
          (unless entry
            ($oops who "~s is not ~a" symbol element-name))
          (fxlogior bits (cdr entry))))
      0 symbols)))

(define aio-register-handle!
  (lambda (st w)
    (aio-debug-check-owner! st)
    (aio-invariant (eq? (aio-handle-state w) st)
      "handle registered with a foreign loop" w)
    (aio-invariant
      (not (hashtable-ref (aio-state-handles st) (aio-handle-id w) #f))
      "handle id was registered twice" w)
    (hashtable-set! (aio-state-handles st) (aio-handle-id w) (weak-cons w #t))
    ((aio-state-guardian st) w)))

(define aio-register-file!
  (lambda (st f)
    (aio-debug-check-owner! st)
    (aio-invariant (eq? (async-file-state f) st)
      "file registered with a foreign loop" f)
    (aio-invariant
      (not (hashtable-ref (aio-state-files st) (async-file-fd f) #f))
      "file descriptor was registered twice" f)
    (hashtable-set! (aio-state-files st) (async-file-fd f) (weak-cons f #t))
    ((aio-state-file-guardian st) f)
    f))

(define aio-unregister-file!
  (lambda (f)
    (aio-debug-check-owner! (async-file-state f))
    (hashtable-delete! (aio-state-files (async-file-state f))
      (async-file-fd f))))

(define aio-register-directory!
  (lambda (st d)
    (aio-debug-check-owner! st)
    (aio-invariant (eq? (async-directory-state d) st)
      "directory registered with a foreign loop" d)
    (hashtable-set! (aio-state-directories st) (async-directory-pointer d)
      (weak-cons d #t))
    ((aio-state-directory-guardian st) d)
    d))

(define aio-unregister-directory!
  (lambda (d)
    (aio-debug-check-owner! (async-directory-state d))
    (hashtable-delete! (aio-state-directories (async-directory-state d))
      (async-directory-pointer d))))

(define aio-lookup-handle
  (lambda (st id)
    (let ([p (hashtable-ref (aio-state-handles st) id #f)])
      (and p
           (let ([w (car p)])
             (and (not (bwp-object? w)) w))))))

;;; register a request; finish runs exactly once at dispatch, canceled or
;;; not, and returns a payload to deliver or #f to stay silent
(define aio-register-request!
  (lambda (st id req)
    (aio-debug-check-owner! st)
    (with-aio-mutex (aio-state-requests-mutex st)
      (aio-invariant (not (hashtable-ref (aio-state-requests st) id #f))
        "native request id was registered twice" id)
      (let ([handle (aio-req-handle req)])
        (when (aio-handle? handle)
          (aio-invariant (eq? (aio-handle-state handle) st)
            "native request uses a handle from another loop" req))
        (when (%async-file? handle)
          (aio-invariant (eq? (async-file-state handle) st)
            "native request uses a file from another loop" req)))
      (hashtable-set! (aio-state-requests st) id req))))

;;; Native libuv objects are touched only by their loop owner.  Foreign
;;; threads enqueue identities or Scheme data and use uv_async_send solely as
;;; the wakeup mechanism.
(define aio-submit-command!
  (lambda (st command)
    (let ([accepted?
           (with-aio-mutex (aio-state-command-mutex st)
             (if (aio-state-closing? st)
                 #f
                 (begin
                   (aio-state-commands-set! st
                     (cons command (aio-state-commands st)))
                   #t)))])
      (when accepted?
        (let ([status (aio-wakeup-send (aio-state-loop st))])
          (when (fx< status 0)
            ($oops 'async-io "cannot wake the owning libuv loop: ~s"
              status))))
      accepted?)))

(define aio-drain-commands!
  (lambda (st)
    (aio-debug-check-owner! st)
    (let ([commands
           (with-aio-mutex (aio-state-command-mutex st)
             (let ([commands (reverse (aio-state-commands st))])
               (aio-state-commands-set! st '())
               commands))])
      (for-each (lambda (command) (command)) commands))))

(define aio-run-on-owner!
  (lambda (st command)
    (if (eq? (current-async-scheduler) (aio-state-owner st))
        (begin
          (aio-debug-check-owner! st)
          (command)
          #t)
        (aio-submit-command! st command))))

(define aio-atomic-box-ref
  (lambda (b)
    (let loop ()
      (let ([v (unbox b)])
        (if (box-cas! b v v) v (loop))))))

(define aio-atomic-box-set-once!
  (lambda (b v)
    (unless (box-cas! b #f v)
      ($oops 'async-io "internal request identity was published twice"))))

(define aio-atomic-box-flag!
  (lambda (b)
    (unless (aio-atomic-box-ref b)
      (box-cas! b #f #t))
    (void)))

;;; finish proc for requests whose native context is a uv_fs_t wrapper
(define aio-fs-finish
  (lambda (gen canceled-gen)
    (lambda (canceled? status aux)
      (let ([payload #f])
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (if canceled?
                (canceled-gen status aux)
                (set! payload (gen status aux))))
          (lambda ()
            (aio-fs-buf-free aux)
            (aio-fs-req-free aux)))
        payload))))

;;; finish proc for dns requests
(define aio-dns-finish
  (lambda (gen)
    (lambda (canceled? status aux)
      (let ([payload (and (not canceled?) (gen status aux))])
        (aio-dns-free aux)
        payload))))

;;; finish proc for requests whose context the shim has already released
(define aio-plain-finish
  (lambda (gen)
    (lambda (canceled? status aux)
      (and (not canceled?) (gen status aux)))))

;;; cancellation of a waiting request: logical cancellation is authoritative;
;;; uv_cancel is used only for the request types that support it
(define aio-cancel-request!
  (lambda (st id)
    (let ([found?
           (with-aio-mutex (aio-state-requests-mutex st)
             (let ([req (hashtable-ref (aio-state-requests st) id #f)])
               (when req (aio-req-canceled?-set! req #t))
               (and req #t)))])
      (when found?
        (aio-submit-command! st
          (lambda ()
            ;; Completion and this command are both dispatched by the loop
            ;; owner, so cancel-data cannot be freed between lookup and use.
            (let ([req
                   (with-aio-mutex (aio-state-requests-mutex st)
                     (hashtable-ref (aio-state-requests st) id #f))])
              (when req
                (let ([cd (aio-req-cancel-data req)])
                  (when cd
                    (case (aio-req-kind req)
                      [(fs) (aio-fs-cancel cd)]
                      [(dns) (aio-dns-cancel cd)]
                      [(nameinfo) (aio-dns-reverse-cancel cd)]
                      [(random) (aio-random-cancel cd)]
                      [else (void)])))))))))))

;;; ------------------------------------------------------------- dispatch

(define aio-dispatch-event
  (lambda (st id kind status aux)
    (aio-debug-check-owner! st)
    (cond
      [(fx= kind AIO-EV-READ) (aio-on-read st id status aux)]
      [(fx= kind AIO-EV-ACCEPT) (aio-on-accept st id status)]
      [(fx= kind AIO-EV-CLOSE) (aio-on-close st id)]
      [(fx= kind AIO-EV-UDP-RECV) (aio-on-udp-recv st id status aux)]
      [(fx= kind AIO-EV-POLL) (aio-on-poll st id status aux)]
      [(fx= kind AIO-EV-PROCESS) (aio-on-process-exit st id status aux)]
      [(or (fx= kind AIO-EV-SIGNAL)
           (fx= kind AIO-EV-FS-EVENT)
           (fx= kind AIO-EV-FS-POLL))
       (aio-on-watch st id kind status aux)]
      [else (aio-on-request st id status aux)])))

(define aio-on-request
  (lambda (st id status aux)
    (let ([req
           (with-aio-mutex (aio-state-requests-mutex st)
             (let ([req (hashtable-ref (aio-state-requests st) id #f)])
               (when req (hashtable-delete! (aio-state-requests st) id))
               req))])
      (aio-invariant req "completion referenced an unknown request id" id)
      (when req
        (let* ([canceled? (or (aio-req-canceled? req) (aio-state-closing? st))]
               [payload ((aio-req-finish req) canceled? status aux)])
          (when payload
            (let ([d (aio-req-deliver req)])
              (when d (d payload)))))))))

(define aio-read-payload
  (lambda (h status aux)
    (cond
      [(fx> status 0)
       (let ([bv (make-bytevector status)])
         (aio-read-copy aux bv status)
         (aio-free aux)
         (cons 'values (list bv)))]
      [(fx= status (aio-eof-code))
       (aio-handle-eof?-set! h #t)
       (cons 'values (list #!eof))]
      [else
       (cons 'raise (aio-io-condition 'read h (aio-handle-path h) status))])))

(define aio-on-read
  (lambda (st id status aux)
    (let ([h (aio-lookup-handle st id)])
      (cond
        [(not h) (when (fx> status 0) (aio-free aux))]
        [else
         (let ([deliveries '()] [free-aux? #f])
           (with-aio-mutex (aio-handle-mutex h)
             (aio-handle-reading?-set! h #f)
             (let ([queue (aio-handle-read-queue h)])
               (cond
                 [(or (aio-state-closing? st) (aio-handle-closing? h))
                  (set! free-aux? (fx> status 0))]
                 [(fx= status (aio-eof-code))
                  (aio-handle-eof?-set! h #t)
                  (set! deliveries
                    (map (lambda (w)
                           (cons (cdr w) (cons 'values (list #!eof))))
                      (aio-queue-drain! queue)))]
                 [else
                  (let ([waiter (aio-queue-pop-live! queue)])
                    (if waiter
                        (set! deliveries
                          (list (cons (cdr waiter)
                                  (aio-read-payload h status aux))))
                        (set! free-aux? (fx> status 0))))]))
             ;; arm the next reader, if any
           (when (and (not (aio-handle-reading? h))
                      (not (aio-handle-closing? h))
                      (not (aio-handle-eof? h))
                      (not (aio-queue-empty? (aio-handle-read-queue h))))
             (aio-handle-reading?-set! h #t)
               (aio-read-start (aio-handle-handle h))))
           (when free-aux? (aio-free aux))
           (for-each
             (lambda (d)
               (unless ((car d) (cdr d))
                 (unless (fx= status (aio-eof-code))
                   (aio-deliver-one-waiter! h
                     (aio-handle-read-queue h) (cdr d)))))
             deliveries))]))))

(define aio-udp-address-values
  (lambda (encoded buf)
    (values (bv->cstring buf)
            (fxmod encoded 65536)
            (quotient encoded 65536))))

(define aio-udp-recv-payload
  (lambda (h status aux)
    (if (fx>= status 0)
        (let ([bv (make-bytevector status)] [addr (make-bytevector 64)])
          (aio-udp-recv-copy aux bv status)
          (let ([encoded (aio-udp-recv-addr aux addr 64)])
            (aio-udp-recv-free aux)
            (if (fx< encoded 0)
                (cons 'raise
                  (aio-io-condition 'udp-receive h (aio-handle-path h)
                    encoded))
                (let-values ([(host port family)
                              (aio-udp-address-values encoded addr)])
                  (cons 'values (list bv host port family))))))
        (cons 'raise
          (aio-io-condition 'udp-receive h (aio-handle-path h) status)))))

(define aio-on-udp-recv
  (lambda (st id status aux)
    (let ([h (aio-lookup-handle st id)])
      (if (not h)
          (when aux (aio-udp-recv-free aux))
          (let ([delivery #f] [free? #f])
            (with-aio-mutex (aio-handle-mutex h)
              (aio-handle-reading?-set! h #f)
              (let ([waiter (aio-queue-pop-live! (aio-handle-read-queue h))])
                (if (or (aio-handle-closing? h) (not waiter))
                    (set! free? (and aux #t))
                    (set! delivery
                      (cons (cdr waiter) (aio-udp-recv-payload h status aux)))))
              (when (and (not (aio-handle-closing? h))
                         (not (aio-queue-empty? (aio-handle-read-queue h))))
                (aio-handle-reading?-set! h #t)
                (aio-udp-recv-start (aio-handle-handle h))))
            (when free? (aio-udp-recv-free aux))
            (when delivery
              (unless ((car delivery) (cdr delivery))
                (aio-deliver-one-waiter! h
                  (aio-handle-read-queue h) (cdr delivery)))))))))

(define aio-poll-event-list
  (lambda (bits)
    (let ([events '()])
      (when (fxlogtest bits 1) (set! events (cons 'readable events)))
      (when (fxlogtest bits 2) (set! events (cons 'writable events)))
      (when (fxlogtest bits 4) (set! events (cons 'disconnect events)))
      (when (fxlogtest bits 8) (set! events (cons 'prioritized events)))
      (reverse events))))

(define aio-on-poll
  (lambda (st id status aux)
    (let ([h (aio-lookup-handle st id)])
      (when h
        (let ([waiter #f])
          (with-aio-mutex (aio-handle-mutex h)
            (aio-handle-reading?-set! h #f)
            (set! waiter (aio-queue-pop-live! (aio-handle-read-queue h))))
          (when (and waiter (not (aio-waiter-dead? (car waiter))))
            ((cdr waiter)
             (if (fx< status 0)
                 (cons 'raise
                   (aio-io-condition 'fd-poll h (aio-handle-path h) status))
                 (cons 'values (list (aio-poll-event-list status)))))))))))

(define aio-on-process-exit
  (lambda (st id status aux)
    (let ([process (aio-lookup-handle st id)])
      (let ([term-signal (aio-process-term-signal aux)])
        (when aux (aio-process-result-free aux))
        (when process
          (let ([waiters '()] [result (cons status term-signal)])
            (with-aio-mutex (aio-handle-mutex process)
              (aio-handle-result-set! process result)
              (set! waiters
                (aio-queue-drain! (aio-handle-accept-queue process))))
            (for-each
              (lambda (waiter)
                (unless (aio-waiter-dead? (car waiter))
                  ((cdr waiter) (cons 'values (list status term-signal)))))
              waiters)))))))

(define aio-watch-payload
  (lambda (h kind status aux)
    (cond
      [(fx= kind AIO-EV-SIGNAL) (cons 'values (list status))]
      [(fx< status 0)
       (cons 'raise
         (aio-io-condition (aio-handle-kind h) h (aio-handle-path h) status))]
      [(fx= kind AIO-EV-FS-EVENT)
       (let ([buf (make-bytevector 4097)])
         (let ([events (aio-fs-event-result-copy aux buf 4097)])
           (if (fx< events 0)
               (cons 'raise
                 (aio-io-condition 'fs-event h (aio-handle-path h) events))
               (cons 'values
                 (list (bv->cstring buf)
                       (append (if (fxlogtest events 1) '(rename) '())
                               (if (fxlogtest events 2) '(change) '())))))))]
      [else
       (let ([stat
              (lambda (current?)
                (define (field i)
                  (aio-fs-poll-result-field aux (if current? 1 0) i))
                (list (cons 'dev (field 0)) (cons 'mode (field 1))
                      (cons 'nlink (field 2)) (cons 'uid (field 3))
                      (cons 'gid (field 4)) (cons 'rdev (field 5))
                      (cons 'ino (field 6)) (cons 'size (field 7))
                      (cons 'blksize (field 8)) (cons 'blocks (field 9))
                      (cons 'atime (cons (field 12) (field 13)))
                      (cons 'mtime (cons (field 14) (field 15)))
                      (cons 'ctime (cons (field 16) (field 17)))))])
         (cons 'values (list (stat #f) (stat #t))))])))

(define aio-on-watch
  (lambda (st id kind status aux)
    (let ([h (aio-lookup-handle st id)] [waiter #f])
      (when h
        (with-aio-mutex (aio-handle-mutex h)
          (aio-handle-reading?-set! h #f)
          (set! waiter (aio-queue-pop-live! (aio-handle-read-queue h)))))
      (let ([payload (and h waiter (aio-watch-payload h kind status aux))])
        (when (fx= kind AIO-EV-FS-EVENT) (aio-fs-event-result-free aux))
        (when (fx= kind AIO-EV-FS-POLL) (aio-fs-poll-result-free aux))
        (when (and waiter (not (aio-waiter-dead? (car waiter))))
          ((cdr waiter) payload))))))

;;; attempt one accept; returns a payload, or #f when nothing is pending
(define aio-attempt-accept
  (lambda (st h)
    (let* ([cid (aio-next-id st)]
           [ch ((if (eq? (aio-handle-kind h) 'pipe-listener)
                    aio-pipe-init
                    aio-tcp-init)
                (aio-state-loop st) cid)])
      (if (= ch 0)
          (cons 'raise (aio-io-condition 'accept h (aio-handle-path h) -12))
          (let ([r (aio-accept (aio-handle-handle h) ch)])
            (cond
              [(fx= r 0)
               (let ([w (make-aio-handle cid ch
                          (if (eq? (aio-handle-kind h) 'pipe-listener)
                              'pipe-stream
                              'tcp-stream)
                          st #f #f (make-aio-os-mutex) #f #f (make-aio-queue) #f #f
                          (make-aio-queue) #f)])
                 (aio-register-handle! st w)
                 (cons 'values (list w)))]
              [(fx= r (aio-eagain-code))
               (aio-handle-close ch)
               #f]
              [else
               (aio-handle-close ch)
               (cons 'raise (aio-io-condition 'accept h (aio-handle-path h) r))]))))))

(define aio-on-accept
  (lambda (st id status)
    (let ([h (aio-lookup-handle st id)])
      (when (and h (not (aio-state-closing? st)))
        (let ([delivery #f])
          (with-aio-mutex (aio-handle-mutex h)
            (let ([waiter (aio-queue-peek-live (aio-handle-accept-queue h))])
              (when waiter
                (let ([payload
                       (if (fx< status 0)
                           (cons 'raise
                             (aio-io-condition 'accept h
                               (aio-handle-path h) status))
                           (aio-attempt-accept st h))])
                  (when payload
                    (aio-queue-pop! (aio-handle-accept-queue h))
                    (set! delivery (cons (cdr waiter) payload)))))))
          (when delivery
            (unless ((car delivery) (cdr delivery))
              (unless (aio-deliver-one-waiter! h
                        (aio-handle-accept-queue h) (cdr delivery))
                ;; Nobody claimed an accepted stream.  Closing it avoids
                ;; retaining a native connection after cancellation won.
                (let ([payload (cdr delivery)])
                  (when (and (eq? (car payload) 'values)
                             (pair? (cdr payload))
                             (aio-handle? (cadr payload)))
                    (aio-close-handle (cadr payload) 'accept)))))))))))

(define aio-on-close
  (lambda (st id)
    (let ([h (aio-lookup-handle st id)])
      (when h
        (with-aio-mutex (aio-handle-mutex h)
          (aio-handle-closed?-set! h #t)))
      (hashtable-delete! (aio-state-handles st) id))))

(define aio-drain-completions!
  (lambda (st)
    (let ([buffer (aio-state-completion-buffer st)])
      (let loop ([n aio-dispatch-bound] [count 0])
        (if (or (fx= n 0)
                (fx= (aio-completion-pop (aio-state-loop st) buffer) 0))
            count
            (begin
              (aio-dispatch-event st
                (bytevector-s64-native-ref buffer 0)
                (bytevector-s64-native-ref buffer 8)
                (bytevector-s64-native-ref buffer 16)
                (bytevector-u64-native-ref buffer 24))
              (loop (fx- n 1) (fx+ count 1))))))))

;;; deferred uv_read_stop requests: set by cancellation nacks that may run
;;; off the scheduler thread, drained by the poll hook on the scheduler thread
(define aio-drain-stop-set!
  (lambda (st)
    (let ([hs (with-aio-mutex (aio-state-stop-mutex st)
                (let ([hs (aio-state-stop-set st)])
                  (aio-state-stop-set-set! st '())
                  hs))])
      (for-each
        (lambda (h)
          (with-aio-mutex (aio-handle-mutex h)
            (when (and (aio-handle-reading? h)
                       (aio-queue-empty? (aio-handle-read-queue h)))
              (aio-handle-reading?-set! h #f)
              ((case (aio-handle-kind h)
                 [(udp) aio-udp-recv-stop]
                 [(poll) aio-poll-stop]
                 [(signal) aio-signal-stop]
                 [(fs-event) aio-fs-event-stop]
                 [(fs-poll) aio-fs-poll-stop]
                 [else aio-read-stop])
               (aio-handle-handle h)))))
        hs))))

(define aio-drain-guardian-objects!
  (lambda (guardian finalize!)
    (let loop ()
      (let ([object (guardian)])
        (when object
          (finalize! object)
          (loop))))))

(define aio-drain-guardian!
  (lambda (st)
    (aio-drain-guardian-objects! (aio-state-guardian st)
      (lambda (w) (aio-close-handle w 'finalized)))))

(define aio-finalize-file!
  (lambda (f)
    (let ([close?
           (with-aio-mutex (async-file-mutex f)
             (if (async-file-closed? f)
                 #f
                 (begin
                   (async-file-closed?-set! f #t)
                   #t)))])
      (when close?
        (aio-fs-close-now (async-file-fd f)))
      (aio-unregister-file! f))))

(define aio-drain-file-guardian!
  (lambda (st)
    (aio-drain-guardian-objects! (aio-state-file-guardian st)
      aio-finalize-file!)))

(define aio-finalize-directory!
  (lambda (d)
    (let ([close?
           (with-aio-mutex (async-directory-mutex d)
             (if (async-directory-closed? d)
                 #f
                 (begin
                   (async-directory-closed?-set! d #t)
                   #t)))])
      (when close? (aio-fs-closedir-now (async-directory-pointer d)))
      (aio-unregister-directory! d))))

(define aio-drain-directory-guardian!
  (lambda (st)
    (aio-drain-guardian-objects! (aio-state-directory-guardian st)
      aio-finalize-directory!)))

;;; ------------------------------------------------------- poll and wake

(define AIO-IDLE-RECHECK-MS 100)
(define AIO-GUARDIAN-POLL-INTERVAL 256)

(define aio-arm-bridge
  (lambda (st sched)
    (let ([timer ($async-scheduler-timers sched)])
      ;; Cross-thread notifications remain the fast path.  The bounded bridge
      ;; interval guarantees that a coalesced native wakeup cannot leave
      ;; Scheme-side work behind a permanently blocking UV_RUN_ONCE.
      (let ([timeout-ms
             (if timer
                 (let* ([deadline ($async-timer-deadline timer)]
                        [delta (max 0 (- deadline ($async-monotonic-us)))])
                   (min AIO-IDLE-RECHECK-MS
                     (quotient (+ delta 999) 1000)))
                 AIO-IDLE-RECHECK-MS)])
        (let ([status
               (aio-bridge-start (aio-state-loop st) timeout-ms)])
          (when (fx< status 0)
            ($oops 'async-io "cannot arm the libuv scheduler bridge: ~s"
              status)))))))

(define aio-poll
  (lambda (sched block?)
    (let ([st ($async-scheduler-io-state sched)])
      (when (and st (not (aio-state-closing? st)))
        (aio-debug-check-owner! st)
        (let* ([old-count (aio-state-poll-count st)]
               [count (if (fx= old-count (most-positive-fixnum))
                          0
                          (fx+ old-count 1))])
          (aio-state-poll-count-set! st count)
          ;; Event dispatch advances on every scheduler turn because a libuv
          ;; operation can require multiple nonblocking uv_run transitions
          ;; while Scheme work keeps the scheduler runnable.
          (when (or block?
                    (fx= (fxmod count AIO-GUARDIAN-POLL-INTERVAL) 0))
            (aio-drain-guardian! st)
            (aio-drain-file-guardian! st)
            (aio-drain-directory-guardian! st))
          (aio-drain-commands! st)
          (aio-drain-stop-set! st)
          (when block? (aio-arm-bridge st sched))
          (aio-loop-run (aio-state-loop st) (if block? 1 0))
          (when block?
            (let ([status (aio-bridge-stop (aio-state-loop st))])
              (when (fx< status 0)
                ($oops 'async-io
                  "cannot disarm the libuv scheduler bridge: ~s" status))))
          (aio-drain-completions! st))))))

;;; ------------------------------------------------------------ shutdown

;;; Orderly native shutdown: close every
;;; handle, cancel in-flight requests, run the loop until the close and
;;; cancellation callbacks complete, then release the loop.  Tasks are
;;; already terminal when this runs, so completions only reclaim native
;;; resources.
(define aio-io-shutdown
  (lambda (sched)
    (let ([st ($async-scheduler-io-state sched)])
      (when (and st (not (aio-state-closing? st)))
        (aio-debug-check-owner! st)
        (with-aio-mutex (aio-state-command-mutex st)
          (aio-state-closing?-set! st #t))
        (aio-drain-commands! st)
        ;; Guardians close wrappers that became unreachable before or during
        ;; shutdown.  Drain on both sides of weak-table enumeration so a GC
        ;; in the enumeration window cannot leave an unclosed native object.
        (aio-drain-guardian! st)
        (aio-drain-file-guardian! st)
        (aio-drain-directory-guardian! st)
        (let-values ([(ks vs) (hashtable-entries (aio-state-handles st))])
          (vector-for-each
            (lambda (p)
              (let ([w (car p)])
                (unless (bwp-object? w)
                  (aio-handle-closing?-set! w #t)
                  (unless (fx= 1 (aio-handle-is-closing (aio-handle-handle w)))
                    (aio-handle-close (aio-handle-handle w))))))
            vs))
        (aio-drain-guardian! st)
        (with-aio-mutex (aio-state-requests-mutex st)
          (let-values ([(ks vs) (hashtable-entries (aio-state-requests st))])
            (vector-for-each
              (lambda (req)
                (aio-req-canceled?-set! req #t)
                (let ([cd (aio-req-cancel-data req)])
                  (when cd
                    (case (aio-req-kind req)
                      [(fs) (aio-fs-cancel cd)]
                      [(dns) (aio-dns-cancel cd)]
                      [(nameinfo) (aio-dns-reverse-cancel cd)]
                      [(random) (aio-random-cancel cd)]
                      [else (void)]))))
              vs)))
        (aio-handle-close (aio-state-wakeup st))
        (aio-handle-close (aio-state-bridge st))
        (let loop ()
          (aio-loop-run (aio-state-loop st) 0)
          (aio-drain-completions! st)
          (when (fx= 1 (aio-loop-alive (aio-state-loop st)))
            (aio-loop-run (aio-state-loop st) 1)
            (aio-drain-completions! st)
            (loop)))
        (let drain ()
          (when (fx= (aio-drain-completions! st) aio-dispatch-bound)
            (drain)))
        ;; No native request remains at this point, so synchronous close cannot
        ;; race an in-flight read, write, or async close request.
        (let-values ([(fds files) (hashtable-entries (aio-state-files st))])
          (vector-for-each
            (lambda (p)
              (let ([f (car p)])
                (unless (bwp-object? f)
                  (aio-finalize-file! f))))
            files))
        (aio-drain-file-guardian! st)
        (let-values ([(pointers directories)
                      (hashtable-entries (aio-state-directories st))])
          (vector-for-each
            (lambda (p)
              (let ([d (car p)])
                (unless (bwp-object? d) (aio-finalize-directory! d))))
            directories))
        (aio-drain-directory-guardian! st)
        (aio-invariant (fx= (hashtable-size (aio-state-requests st)) 0)
          "loop shutdown retained native requests" st)
        (aio-invariant (fx= (hashtable-size (aio-state-handles st)) 0)
          "loop shutdown retained native handles" st)
        (aio-invariant (fx= (hashtable-size (aio-state-files st)) 0)
          "loop shutdown retained native files" st)
        (aio-invariant (fx= (hashtable-size (aio-state-directories st)) 0)
          "loop shutdown retained native directories" st)
        (let ([status (aio-loop-destroy (aio-state-loop st))])
          (unless (fx= status 0)
            ($oops 'async-io-shutdown
              "libuv loop close failed with status ~s" status)))))))

;;; -------------------------------------------------------- state setup

(define aio-ensure-state!
  (lambda (who)
    (aio-resolve-kernel!)
    (let ([sched (current-async-scheduler)])
      (unless sched
        ($oops who "not in an async scheduler"))
      (when ($async-scheduler-virtual? sched)
        ($oops who "asynchronous I/O is not available on a virtual-clock scheduler"))
      (or ($async-scheduler-io-state sched)
          (let ([loop (aio-loop-open)])
            (when (= loop 0)
              ($oops who "cannot create a libuv loop"))
            (let ([wakeup (aio-wakeup-init loop)])
              (when (= wakeup 0)
                (aio-loop-destroy loop)
                ($oops who "cannot initialize the libuv wakeup handle"))
              (let ([bridge (aio-bridge-init loop)])
                (when (= bridge 0)
                  (aio-handle-close wakeup)
                  (let drain ()
                    (when (fx= 1 (aio-loop-alive loop))
                      (aio-loop-run loop 0)
                      (drain)))
                  (let ([buffer (make-bytevector 32)])
                    (let discard ()
                      (when (fx= (aio-completion-pop loop buffer) 1)
                        (discard))))
                  (aio-loop-destroy loop)
                  ($oops who "cannot initialize the libuv timer handle"))
                (let ([st (make-aio-state sched loop wakeup bridge
                        1 (make-eq-hashtable) (make-aio-os-mutex)
                        (make-eq-hashtable) (make-eq-hashtable)
                        (make-eq-hashtable)
                        (make-bytevector 32) '() (make-aio-os-mutex)
                        '() (make-aio-os-mutex) 0 #f
                        (make-guardian) (make-guardian) (make-guardian))])
                  ($async-scheduler-io-state-set! sched st)
                  ($async-scheduler-poll-proc-set! sched aio-poll)
                  ($async-scheduler-wake-proc-set! sched
                    (lambda ()
                      (with-aio-mutex (aio-state-command-mutex st)
                        (unless (aio-state-closing? st)
                          (let ([status (aio-wakeup-send loop)])
                            (when (fx< status 0)
                              ($oops 'async-io
                                "cannot wake the libuv scheduler: ~s"
                                status)))))))
                  st))))))))
