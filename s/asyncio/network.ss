;;; ------------------------------------------------------------ operations

;;; A libuv wait resumes once on the scheduler that owns its registration.
;;; The scheduler clears the pin when it claims that turn, so the task is
;;; eligible for ordinary work stealing at its next suspension.
(define aio-make-operation
  (case-lambda
    [(try block)
     (aio-make-operation try block (lambda (values) values)
       (lambda (ss) (void)))]
    [(try block wrap)
     (aio-make-operation try block wrap (lambda (ss) (void)))]
    [(try block wrap nack)
     (make-operation try
       (lambda (ss deliver)
         ($async-pin-current-wait!)
         (block ss deliver))
       wrap nack)]))

;;; ------------------------------------------------------------ handles

;;; owner-thread close: wakes pending readers/writers/acceptors with a
;;; closed-handle condition, then closes the native handle
(define aio-cancel-handle-requests!
  (lambda (w operation)
    (let ([deliveries '()])
      (with-aio-mutex (aio-state-requests-mutex (aio-handle-state w))
        (let-values ([(ids reqs)
                      (hashtable-entries
                        (aio-state-requests (aio-handle-state w)))])
          (vector-for-each
            (lambda (req)
              (when (and (eq? (aio-req-handle req) w)
                         (not (aio-req-canceled? req)))
                (let ([deliver (aio-req-deliver req)])
                  (aio-req-canceled?-set! req #t)
                  (aio-req-deliver-set! req #f)
                  (when deliver
                    (set! deliveries (cons deliver deliveries))))))
            reqs)))
      (let ([payload (cons 'raise (aio-closed-condition operation w))])
        (for-each (lambda (deliver) (deliver payload)) deliveries)))))

(define aio-close-handle
  (lambda (w operation)
    (aio-debug-check-owner! (aio-handle-state w))
    (let-values ([(close? waiters)
                  (with-aio-mutex (aio-handle-mutex w)
                    (if (aio-handle-closing? w)
                        (values #f '())
                        (begin
                          (aio-handle-closing?-set! w #t)
                          (let ([waiters
                                 (append
                                   (aio-queue-drain! (aio-handle-read-queue w))
                                   (aio-queue-drain!
                                     (aio-handle-accept-queue w)))])
                            (values #t waiters)))))])
      (when close?
        (let ([payload (cons 'raise (aio-closed-condition operation w))])
          (for-each
            (lambda (waiter)
              (unless (aio-waiter-dead? (car waiter))
                ((cdr waiter) payload)))
            waiters))
        (aio-cancel-handle-requests! w operation)
        (aio-handle-close (aio-handle-handle w))))))

;;; ------------------------------------------------------------- streams

(define aio-check-stream
  (lambda (who s)
    (unless (async-stream? s)
      ($oops who "~s is not an async stream" s))))

(define aio-check-stream-unowned
  (lambda (who s)
    (aio-check-stream who s)
    (with-aio-mutex (aio-handle-mutex s)
      (when (aio-handle-port-owned? s)
        ($oops who "async stream ownership has been transferred to a port")))))

(define aio-check-handle-scope!
  (lambda (who h)
    (aio-check-state-scope! who (aio-handle-state h)
      "async handle belongs to another scheduler group")))

(define aio-check-stream-access!
  (lambda (who s allow-owned?)
    (aio-check-stream who s)
    (aio-check-handle-scope! who s)
    (unless allow-owned?
      (with-aio-mutex (aio-handle-mutex s)
        (when (aio-handle-port-owned? s)
          ($oops who "async stream ownership has been transferred to a port"))))))

(define aio-claim-stream-for-port!
  (lambda (who s)
    (aio-check-stream who s)
    (aio-check-handle-scope! who s)
    (with-aio-mutex (aio-handle-mutex s)
      (when (aio-handle-port-owned? s)
        ($oops who "async stream ownership has already been transferred to a port"))
      (when (aio-handle-closing? s)
        (raise (aio-closed-condition who s)))
      (aio-handle-port-owned?-set! s #t))))

(define aio-close-owned-handle
  (lambda (who h)
    (aio-check-handle-scope! who h)
    (aio-run-on-owner! (aio-handle-state h)
      (lambda () (aio-close-handle h 'close)))
    (void)))

(define aio-start-stream-read!
  (lambda (s)
    (with-aio-mutex (aio-handle-mutex s)
      (when (and (not (aio-handle-reading? s))
                 (not (aio-handle-closing? s))
                 (not (aio-handle-eof? s))
                 (not (aio-queue-empty? (aio-handle-read-queue s))))
        (aio-handle-reading?-set! s #t)
        (aio-read-start (aio-handle-handle s))))))

(define %stream-read-operation
  (lambda (s . allow-owned-option)
    (aio-check-stream 'stream-read-operation s)
    (let ([token (list 'stream-read-operation)]
          [allow-owned? (and (pair? allow-owned-option)
                             (car allow-owned-option))])
      (aio-make-operation
        (lambda (ss)
          (aio-check-stream-access! 'stream-read-operation s allow-owned?)
        (with-aio-mutex (aio-handle-mutex s)
          (cond
            [(aio-handle-eof? s) (cons 'values (list #!eof))]
            [(aio-handle-closing? s)
             (cons 'raise (aio-closed-condition 'read s))]
            [else #f])))
        (lambda (ss deliver)
          (aio-check-stream-access! 'stream-read-operation s allow-owned?)
          (let ([result
                 (with-aio-mutex (aio-handle-mutex s)
                   (cond
                     [(aio-handle-eof? s)
                      (cons 'immediate (cons 'values (list #!eof)))]
                     [(aio-handle-closing? s)
                      (cons 'immediate
                        (cons 'raise (aio-closed-condition 'read s)))]
                     [else
                      (let ([node
                             (aio-queue-push! (aio-handle-read-queue s)
                               (cons ss deliver))])
                        ($async-sync-slot-set! ss token node)
                        '(blocked))]))])
            (if (eq? (car result) 'blocked)
                (begin
                  (aio-run-on-owner! (aio-handle-state s)
                    (lambda () (aio-start-stream-read! s)))
                  (list 'read (aio-handle-id s)))
                (begin (deliver (cdr result)) #f))))
        (lambda (vals) vals)
        (lambda (ss)
          (let ([node ($async-sync-slot-ref ss token #f)])
            (when node
              ($async-sync-slot-delete! ss token)
              (with-aio-mutex (aio-handle-mutex s)
                (aio-queue-remove! (aio-handle-read-queue s) node))))
          (let ([st (aio-handle-state s)])
            (with-aio-mutex (aio-state-stop-mutex st)
              (aio-state-stop-set-set! st
                (cons s (aio-state-stop-set st))))))))))

(define %stream-write-operation
  (lambda (s bv . allow-owned-option)
    (aio-check-stream 'stream-write-operation s)
    (unless (bytevector? bv)
      ($oops 'stream-write-operation "~s is not a bytevector" bv))
    (let ([token (list 'stream-write-operation)]
          [allow-owned? (and (pair? allow-owned-option)
                             (car allow-owned-option))])
      (aio-make-operation
        (lambda (ss)
          (aio-check-stream-access! 'stream-write-operation s allow-owned?)
          (with-aio-mutex (aio-handle-mutex s)
            (and (aio-handle-closing? s)
                 (cons 'raise (aio-closed-condition 'write s)))))
        (lambda (ss deliver)
          (aio-check-stream-access! 'stream-write-operation s allow-owned?)
          (if (with-aio-mutex (aio-handle-mutex s)
                (aio-handle-closing? s))
              (begin
                (deliver (cons 'raise (aio-closed-condition 'write s)))
                #f)
              (let* ([st (aio-handle-state s)]
                     [id-box (box #f)]
                     [canceled-box (box #f)]
                     [attempt (vector st id-box canceled-box)]
                     [len (bytevector-length bv)])
                ($async-sync-slot-set! ss token attempt)
                (if (aio-run-on-owner! st
                      (lambda ()
                        (cond
                          [(or (aio-atomic-box-ref canceled-box)
                               (aio-waiter-dead? ss))
                           (void)]
                          [(with-aio-mutex (aio-handle-mutex s)
                             (aio-handle-closing? s))
                           (deliver
                             (cons 'raise (aio-closed-condition 'write s)))]
                          [else
                          (let* ([id (aio-next-id st)]
                                 [r (aio-write
                                      (aio-handle-handle s) bv len id)])
                            (aio-atomic-box-set-once! id-box id)
                            (if (< r 0)
                                (deliver
                                  (cons 'raise
                                    (aio-io-condition 'write s
                                      (aio-handle-path s) r)))
                                (begin
                                  (aio-register-request! st id
                                    (make-aio-req 'write s deliver #f
                                      (aio-plain-finish
                                        (lambda (status aux)
                                          (if (fx= status 0)
                                              (cons 'values (list len))
                                              (cons 'raise
                                                (aio-io-condition 'write s
                                                  (aio-handle-path s) status)))))
                                      #f))
                                  (when (or (aio-atomic-box-ref canceled-box)
                                            (aio-waiter-dead? ss))
                                    (aio-cancel-request! st id)))))])))
                    (list 'write (aio-handle-id s))
                    (begin
                      (deliver (cons 'raise (aio-closed-condition 'write s)))
                      #f)))))
        (lambda (vals) vals)
        (aio-request-nack token)))))

(define stream-shutdown-operation
  (lambda (s)
    (aio-check-stream 'stream-shutdown s)
    (let ([token (list 'stream-shutdown-operation)])
      (aio-make-operation
        (lambda (ss)
          (aio-check-stream-access! 'stream-shutdown s #f)
          (with-aio-mutex (aio-handle-mutex s)
            (and (aio-handle-closing? s)
                 (cons 'raise (aio-closed-condition 'shutdown s)))))
        (lambda (ss deliver)
          (aio-check-stream-access! 'stream-shutdown s #f)
          (if (with-aio-mutex (aio-handle-mutex s)
                (aio-handle-closing? s))
              (begin
                (deliver (cons 'raise (aio-closed-condition 'shutdown s)))
                #f)
              (let* ([st (aio-handle-state s)]
                     [id-box (box #f)]
                     [canceled-box (box #f)]
                     [attempt (vector st id-box canceled-box)])
                ($async-sync-slot-set! ss token attempt)
                (if (aio-run-on-owner! st
                      (lambda ()
                        (cond
                          [(or (aio-atomic-box-ref canceled-box)
                               (aio-waiter-dead? ss))
                           (void)]
                          [(with-aio-mutex (aio-handle-mutex s)
                             (aio-handle-closing? s))
                           (deliver
                             (cons 'raise (aio-closed-condition 'shutdown s)))]
                          [else
                          (let* ([id (aio-next-id st)]
                                 [r (aio-shutdown (aio-handle-handle s) id)])
                            (aio-atomic-box-set-once! id-box id)
                            (if (< r 0)
                                (deliver
                                  (cons 'raise
                                    (aio-io-condition 'shutdown s
                                      (aio-handle-path s) r)))
                                (begin
                                  (aio-register-request! st id
                                    (make-aio-req 'shutdown s deliver #f
                                      (aio-plain-finish
                                        (lambda (status aux)
                                          (if (fx= status 0)
                                              (cons 'values '())
                                              (cons 'raise
                                                (aio-io-condition 'shutdown s
                                                  (aio-handle-path s) status)))))
                                      #f))
                                  (when (or (aio-atomic-box-ref canceled-box)
                                            (aio-waiter-dead? ss))
                                    (aio-cancel-request! st id)))))])))
                    (list 'shutdown (aio-handle-id s))
                    (begin
                      (deliver
                        (cons 'raise (aio-closed-condition 'shutdown s)))
                      #f)))))
        (lambda (vals) vals)
        (aio-request-nack token)))))

;;; The request identity belongs to one perform, not to the reusable operation.
(define aio-request-nack
  (lambda (token)
    (lambda (ss)
      (let ([attempt ($async-sync-slot-ref ss token #f)])
        (when attempt
          ($async-sync-slot-delete! ss token)
          (if (vector? attempt)
              (let ([st (vector-ref attempt 0)]
                    [id-box (vector-ref attempt 1)])
                (aio-atomic-box-flag! (vector-ref attempt 2))
                (let ([id (aio-atomic-box-ref id-box)])
                  (when id (aio-cancel-request! st id))))
              (aio-cancel-request! (car attempt) (cdr attempt))))))))

;;; ------------------------------------------------------------- tcp

(define aio-check-host-port
  (lambda (who host port)
    (unless (string? host) ($oops who "~s is not a string" host))
    (unless (and (fixnum? port) (fx>= port 0) (fx<= port 65535))
      ($oops who "~s is not a valid port number" port))))

(define %tcp-listen
  (case-lambda
    [(host port) (%tcp-listen host port 128)]
    [(host port backlog)
     (aio-check-host-port 'tcp-listen host port)
     (unless (and (fixnum? backlog) (fx> backlog 0))
       ($oops 'tcp-listen "~s is not a positive fixnum" backlog))
     (let* ([st (aio-ensure-state! 'tcp-listen)]
            [id (aio-next-id st)]
            [h (aio-tcp-init (aio-state-loop st) id)])
       (when (= h 0)
         ($oops 'tcp-listen "cannot allocate a tcp handle"))
       (let ([w (make-aio-handle id h 'tcp-listener st
                  (format "~a:~a" host port) #f (make-aio-os-mutex)
                  #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
         (define (fail r)
           (aio-handle-close h)
           (raise (aio-io-condition 'listen w (aio-handle-path w) r)))
         (let ([r (aio-tcp-bind h host port)])
           (when (fx< r 0) (fail r)))
         (let ([r (aio-listen-start h backlog)])
           (when (fx< r 0) (fail r)))
         (aio-register-handle! st w)
         w))]))

(define %tcp-accept-operation
  (lambda (listener)
    (unless (tcp-listener? listener)
      ($oops 'tcp-accept-operation "~s is not a tcp listener" listener))
    (let ([st (aio-handle-state listener)]
          [token (list 'tcp-accept-operation)])
      (aio-make-operation
        (lambda (ss)
          (aio-check-handle-scope! 'tcp-accept-operation listener)
          (with-aio-mutex (aio-handle-mutex listener)
            (cond
              [(aio-handle-closing? listener)
               (cons 'raise (aio-closed-condition 'accept listener))]
              [else #f])))
        (lambda (ss deliver)
          (aio-check-handle-scope! 'tcp-accept-operation listener)
          (let ([payload
                 (with-aio-mutex (aio-handle-mutex listener)
                   (if (aio-handle-closing? listener)
                       (cons 'raise (aio-closed-condition 'accept listener))
                       (begin
                         ($async-sync-slot-set! ss token
                           (aio-queue-push!
                             (aio-handle-accept-queue listener)
                             (cons ss deliver)))
                         #f)))])
            (if (not payload)
                (begin
                  (aio-run-on-owner! st
                    (lambda ()
                      ;; A connection may already be pending before libuv
                      ;; reports another listener event.
                      (aio-on-accept st (aio-handle-id listener) 0)))
                  (list 'accept (aio-handle-id listener)))
                (begin (deliver payload) #f))))
        (lambda (vals) vals)
        (lambda (ss)
          (let ([node ($async-sync-slot-ref ss token #f)])
            (when node
              ($async-sync-slot-delete! ss token)
              (with-aio-mutex (aio-handle-mutex listener)
                (aio-queue-remove!
                  (aio-handle-accept-queue listener) node)))))))))

(define %tcp-connect-operation
  (lambda (host port)
    (aio-check-host-port 'tcp-connect-operation host port)
    (let ([token (list 'tcp-connect-operation)])
      (aio-make-operation
        (lambda (ss) #f)
        (lambda (ss deliver)
          (let* ([st (aio-ensure-state! 'tcp-connect-operation)]
                 [id (aio-next-id st)]
                 [h (aio-tcp-init (aio-state-loop st) id)])
            ($async-sync-slot-set! ss token (cons st id))
            (if (= h 0)
                (begin
                  (deliver
                    (cons 'raise
                      (aio-io-condition 'connect #f (format "~a:~a" host port) -12)))
                  #f)
                (let ([w (make-aio-handle id h 'tcp-stream st
                           (format "~a:~a" host port) #f (make-aio-os-mutex)
                           #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
                  (aio-register-handle! st w)
                  (let ([r (aio-tcp-connect h host port id)])
                    (if (< r 0)
                        (begin
                          (aio-close-handle w 'connect)
                          (deliver
                            (cons 'raise
                              (aio-io-condition 'connect w (aio-handle-path w) r)))
                          #f)
                        (begin
                          (aio-register-request! st id
                            (make-aio-req 'connect w deliver #f
                              (lambda (canceled? status aux)
                                (cond
                                  [canceled?
                                   (aio-close-handle w 'connect)
                                   #f]
                                  [(fx= status 0)
                                   (cons 'values (list w))]
                                  [else
                                   (aio-close-handle w 'connect)
                                   (cons 'raise
                                     (aio-io-condition 'connect w
                                       (aio-handle-path w) status))]))
                              #f))
                          (list 'connect id))))))))
        (lambda (vals) vals)
        ;; a connect cannot be canceled in libuv; the completion closes the
        ;; handle and is dropped because the request is marked canceled
        (aio-request-nack token)))))

;;; ------------------------------------------------------ local-domain

(define %pipe-listen
  (case-lambda
    [(path) (%pipe-listen path 128)]
    [(path backlog)
     (unless (string? path) ($oops 'pipe-listen "~s is not a string" path))
     (unless (and (fixnum? backlog) (fx> backlog 0))
       ($oops 'pipe-listen "~s is not a positive fixnum" backlog))
     (let* ([st (aio-ensure-state! 'pipe-listen)]
            [id (aio-next-id st)]
            [h (aio-pipe-init (aio-state-loop st) id)])
       (when (= h 0)
         ($oops 'pipe-listen "cannot allocate a pipe handle"))
       (let ([w (make-aio-handle id h 'pipe-listener st path #f (make-aio-os-mutex)
                  #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
         (define (fail r)
           (aio-handle-close h)
           (raise (aio-io-condition 'listen w path r)))
         (let ([r (aio-pipe-bind h path)])
           (when (fx< r 0) (fail r)))
         (let ([r (aio-listen-start h backlog)])
           (when (fx< r 0) (fail r)))
         (aio-register-handle! st w)
         w))]))

(define %pipe-connect-operation
  (lambda (path)
    (unless (string? path)
      ($oops 'pipe-connect-operation "~s is not a string" path))
    (let ([token (list 'pipe-connect-operation)])
      (aio-make-operation
        (lambda (ss) #f)
        (lambda (ss deliver)
          (let* ([st (aio-ensure-state! 'pipe-connect-operation)]
                 [id (aio-next-id st)]
                 [h (aio-pipe-init (aio-state-loop st) id)])
            ($async-sync-slot-set! ss token (cons st id))
            (if (= h 0)
                (begin
                  (deliver
                    (cons 'raise (aio-io-condition 'connect #f path -12)))
                  #f)
                (let ([w (make-aio-handle id h 'pipe-stream st path #f (make-aio-os-mutex)
                           #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
                  (aio-register-handle! st w)
                  (aio-register-request! st id
                    (make-aio-req 'connect w deliver #f
                      (lambda (canceled? status aux)
                        (cond
                          [canceled?
                           (aio-close-handle w 'connect)
                           #f]
                          [(fx= status 0)
                           (cons 'values (list w))]
                          [else
                           (aio-close-handle w 'connect)
                           (cons 'raise
                             (aio-io-condition 'connect w path status))]))
                      #f))
                  (aio-pipe-connect h path id)
                  (list 'connect id)))))
        (lambda (vals) vals)
        (aio-request-nack token)))))

;;; ---------------------------------------------------------------- dns

(define %dns-lookup-operation
  (case-lambda
    [(node) (%dns-lookup-operation node #f)]
    [(node service)
     (unless (string? node)
       ($oops 'dns-lookup-operation "~s is not a string" node))
     (unless (or (not service) (string? service))
       ($oops 'dns-lookup-operation "~s is not a string or #f" service))
     (let ([token (list 'dns-lookup-operation)])
       (aio-make-operation
         (lambda (ss) #f)
         (lambda (ss deliver)
           (let* ([st (aio-ensure-state! 'dns-lookup-operation)]
                  [id (aio-next-id st)]
                  [r (aio-dns-lookup (aio-state-loop st) node
                       (or service "") id)])
             ($async-sync-slot-set! ss token (cons st id))
             (if (< r 0)
                 (begin
                   (deliver
                     (cons 'raise (aio-io-condition 'dns #f node r)))
                   #f)
                 (begin
                   (aio-register-request! st id
                     (make-aio-req 'dns #f deliver r
                       (aio-dns-finish
                         (lambda (status aux)
                           (if (fx= status 0)
                               (let ([n (aio-dns-count aux)])
                                 (let loop ([i 0] [acc '()])
                                   (if (fx= i n)
                                       (cons 'values (list (reverse acc)))
                                       (let ([buf (make-bytevector 64)])
                                         (let ([fp (aio-dns-addr aux i buf 64)])
                                           (if (< fp 0)
                                               (loop (fx+ i 1) acc)
                                               (loop (fx+ i 1)
                                                     (cons (list (bv->cstring buf)
                                                             (fxmod fp 65536)
                                                             (quotient fp 65536))
                                                           acc))))))))
                               (cons 'raise (aio-io-condition 'dns #f node status)))))
                       #f))
                   (list 'dns id)))))
         (lambda (vals) vals)
         (aio-request-nack token)))]))

;;; ------------------------------------------------------------------ udp

(define aio-check-udp
  (lambda (who socket)
    (unless (and (aio-handle? socket)
                 (eq? (aio-handle-kind socket) 'udp))
      ($oops who "~s is not a UDP socket" socket))))

(define aio-check-udp-open!
  (lambda (who socket)
    (aio-check-udp who socket)
    (aio-check-handle-scope! who socket)
    (when (with-aio-mutex (aio-handle-mutex socket)
            (aio-handle-closing? socket))
      (raise (aio-closed-condition who socket)))))

(define aio-udp-bind-flag-bits
  (lambda (who flags)
    (aio-symbols->flag-bits who flags "UDP bind flags" "a UDP bind flag"
      '((ipv6-only . 1) (reuse-address . 2)))))

(define %udp-open
  (case-lambda
    [()
     (let* ([st (aio-ensure-state! 'udp-open)]
            [id (aio-next-id st)]
            [h (aio-udp-init (aio-state-loop st) id)])
       (when (= h 0) ($oops 'udp-open "cannot allocate a UDP handle"))
       (let ([socket
              (make-aio-handle id h 'udp st #f #f (make-aio-os-mutex)
                #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
         (aio-register-handle! st socket)
         socket))]
    [(host port) (%udp-open host port '())]
    [(host port flags)
     (aio-check-host-port 'udp-open host port)
     (let ([socket (%udp-open)])
       (let ([r (aio-udp-bind (aio-handle-handle socket) host port
                  (aio-udp-bind-flag-bits 'udp-open flags))])
         (if (fx< r 0)
             (begin
               (aio-close-handle socket 'udp-open)
               (raise (aio-io-condition 'udp-bind socket
                        (format "~a:~a" host port) r)))
             socket)))]))

(define aio-start-udp-recv!
  (lambda (socket)
    (let ([delivery #f])
      (with-aio-mutex (aio-handle-mutex socket)
        (when (and (not (aio-handle-reading? socket))
                   (not (aio-handle-closing? socket))
                   (not (aio-queue-empty? (aio-handle-read-queue socket))))
          (aio-handle-reading?-set! socket #t)
          (let ([r (aio-udp-recv-start (aio-handle-handle socket))])
            (when (fx< r 0)
              (aio-handle-reading?-set! socket #f)
              (let ([waiter
                     (aio-queue-pop-live! (aio-handle-read-queue socket))])
                (when waiter
                  (set! delivery
                    (cons (cdr waiter)
                      (cons 'raise
                        (aio-io-condition 'udp-receive socket
                          (aio-handle-path socket) r))))))))))
      (when delivery ((car delivery) (cdr delivery))))))

(define %udp-receive-operation
  (lambda (socket)
    (aio-check-udp 'udp-receive-operation socket)
    (let ([token (list 'udp-receive-operation)])
      (aio-make-operation
      (lambda (ss)
        (aio-check-udp-open! 'udp-receive-operation socket)
        #f)
      (lambda (ss deliver)
        (aio-check-udp-open! 'udp-receive-operation socket)
        (let ([payload
               (with-aio-mutex (aio-handle-mutex socket)
                 (if (aio-handle-closing? socket)
                     (cons 'raise
                       (aio-closed-condition 'udp-receive socket))
                     (begin
                       ($async-sync-slot-set! ss token
                         (aio-queue-push! (aio-handle-read-queue socket)
                           (cons ss deliver)))
                       #f)))])
          (when (not payload)
            (aio-run-on-owner! (aio-handle-state socket)
              (lambda () (aio-start-udp-recv! socket))))
          (if payload
              (begin (deliver payload) #f)
              (list 'udp-receive (aio-handle-id socket)))))
      (lambda (vals) vals)
      (lambda (ss)
        (let ([node ($async-sync-slot-ref ss token #f)])
          (when node
            ($async-sync-slot-delete! ss token)
            (with-aio-mutex (aio-handle-mutex socket)
              (aio-queue-remove! (aio-handle-read-queue socket) node))))
        (let ([st (aio-handle-state socket)])
          (with-aio-mutex (aio-state-stop-mutex st)
            (aio-state-stop-set-set! st
              (cons socket (aio-state-stop-set st))))))))))

(define %udp-send-operation
  (case-lambda
    [(socket bv) (%udp-send-operation socket bv "" 0)]
    [(socket bv host port)
     (aio-check-udp 'udp-send-operation socket)
     (unless (bytevector? bv)
       ($oops 'udp-send-operation "~s is not a bytevector" bv))
     (unless (string=? host "")
       (aio-check-host-port 'udp-send-operation host port))
     (let ([token (list 'udp-send-operation)])
       (aio-make-operation
         (lambda (ss)
           (aio-check-udp-open! 'udp-send-operation socket)
           #f)
         (lambda (ss deliver)
           (aio-check-udp-open! 'udp-send-operation socket)
           (let* ([st (aio-handle-state socket)]
                  [id-box (box #f)]
                  [canceled-box (box #f)]
                  [len (bytevector-length bv)])
             ($async-sync-slot-set! ss token
               (vector st id-box canceled-box))
             (if (aio-run-on-owner! st
                   (lambda ()
                     (unless (or (aio-atomic-box-ref canceled-box)
                                 (aio-waiter-dead? ss))
                       (let* ([id (aio-next-id st)]
                              [r (aio-udp-send (aio-handle-handle socket)
                                   bv len host port id)])
                         (aio-atomic-box-set-once! id-box id)
                         (if (fx< r 0)
                             (deliver (cons 'raise
                                        (aio-io-condition 'udp-send socket
                                          (aio-handle-path socket) r)))
                             (begin
                               (aio-register-request! st id
                                 (make-aio-req 'udp-send socket deliver #f
                                   (aio-plain-finish
                                     (lambda (status aux)
                                       (if (fx= status 0)
                                           (cons 'values (list len))
                                           (cons 'raise
                                             (aio-io-condition 'udp-send socket
                                               (aio-handle-path socket)
                                               status)))))
                                   #f))
                               (when (or (aio-atomic-box-ref canceled-box)
                                         (aio-waiter-dead? ss))
                                 (aio-cancel-request! st id))))))))
                 (list 'udp-send (aio-handle-id socket))
                 (begin
                   (deliver (cons 'raise
                              (aio-closed-condition 'udp-send socket)))
                   #f))))
         (lambda (vals) vals)
         (aio-request-nack token)))]))

(define aio-udp-control
  (lambda (who socket thunk)
    (aio-check-udp-open! who socket)
    (let ([r (thunk)])
      (when (fx< r 0)
        (raise (aio-io-condition who socket (aio-handle-path socket) r))))))

(define aio-udp-address-list
  (lambda (who socket peer?)
    (aio-check-udp-open! who socket)
    (let ([buf (make-bytevector 64)])
      (let ([encoded (aio-udp-address (aio-handle-handle socket)
                       (if peer? 1 0) buf 64)])
        (if (fx< encoded 0)
            (raise (aio-io-condition who socket (aio-handle-path socket)
                     encoded))
            (let-values ([(host port family)
                          (aio-udp-address-values encoded buf)])
              (list host port family)))))))

;;; ----------------------------------------- reverse DNS, random, fd poll

(define aio-nameinfo-flag-bits
  (lambda (who flags)
    (aio-symbols->flag-bits who flags "reverse-DNS flags"
      "a reverse-DNS flag"
      '((name-required . 1) (numeric-host . 2) (numeric-service . 4)))))

(define %dns-reverse-operation
  (case-lambda
    [(host port) (%dns-reverse-operation host port '())]
    [(host port flags)
     (aio-check-host-port 'dns-reverse-operation host port)
     (let ([bits (aio-nameinfo-flag-bits 'dns-reverse-operation flags)]
           [token (list 'dns-reverse-operation)])
       (aio-make-operation
         (lambda (ss) #f)
         (lambda (ss deliver)
           (let* ([st (aio-ensure-state! 'dns-reverse-operation)]
                  [id (aio-next-id st)]
                  [ctx (aio-dns-reverse (aio-state-loop st) host port bits id)])
             (if (fx< ctx 0)
                 (begin
                   (deliver
                     (cons 'raise
                       (aio-io-condition 'reverse-dns #f host ctx)))
                   #f)
                 (begin
                   ($async-sync-slot-set! ss token (cons st id))
                   (aio-register-request! st id
                     (make-aio-req 'nameinfo #f deliver ctx
                       (lambda (canceled? status aux)
                         (dynamic-wind
                           (lambda () (void))
                           (lambda ()
                             (and (not canceled?)
                                  (if (fx= status 0)
                                      (let ([host-buf (make-bytevector 1024)]
                                            [service-buf (make-bytevector 256)])
                                        (let ([hr (aio-dns-reverse-copy aux 0
                                                    host-buf 1024)]
                                              [sr (aio-dns-reverse-copy aux 1
                                                    service-buf 256)])
                                          (if (or (fx< hr 0) (fx< sr 0))
                                              (cons 'raise
                                                (aio-io-condition 'reverse-dns
                                                  #f host
                                                  (if (fx< hr 0) hr sr)))
                                              (cons 'values
                                                (list (bv->cstring host-buf)
                                                      (bv->cstring service-buf))))))
                                      (cons 'raise
                                        (aio-io-condition 'reverse-dns #f host
                                          status)))))
                           (lambda () (aio-dns-reverse-free aux))))
                       #f))
                   (list 'reverse-dns id)))))
         (lambda (vals) vals)
         (aio-request-nack token)))]))

(define %random-bytevector-operation
  (lambda (length)
    (unless (and (fixnum? length) (fx>= length 0))
      ($oops 'random-bytevector-operation
        "~s is not a nonnegative fixnum" length))
    (let ([token (list 'random-bytevector-operation)])
      (aio-make-operation
        (lambda (ss) #f)
        (lambda (ss deliver)
          (let* ([st (aio-ensure-state! 'random-bytevector-operation)]
                 [id (aio-next-id st)]
                 [ctx (aio-random (aio-state-loop st) length id)])
            (if (fx< ctx 0)
                (begin
                  (deliver
                    (cons 'raise (aio-io-condition 'random #f #f ctx)))
                  #f)
                (begin
                  ($async-sync-slot-set! ss token (cons st id))
                  (aio-register-request! st id
                    (make-aio-req 'random #f deliver ctx
                      (lambda (canceled? status aux)
                        (dynamic-wind
                          (lambda () (void))
                          (lambda ()
                            (and (not canceled?)
                                 (if (fx= status 0)
                                     (let ([bv (make-bytevector length)])
                                       (aio-random-copy aux bv)
                                       (cons 'values (list bv)))
                                     (cons 'raise
                                       (aio-io-condition 'random #f #f status)))))
                          (lambda () (aio-random-free aux))))
                      #f))
                  (list 'random id))))
        (lambda (vals) vals)
        (aio-request-nack token))))))

(define aio-poll-event-bits
  (lambda (who events)
    (let ([bits
           (aio-symbols->flag-bits who events "poll events" "a poll event"
             '((readable . 1) (writable . 2) (disconnect . 4)
               (prioritized . 8)))])
      (when (fx= bits 0) ($oops who "poll event list is empty"))
      bits)))

(define %fd-poll-open
  (lambda (fd)
    (unless (and (fixnum? fd) (fx>= fd 0))
      ($oops 'fd-poll-open "~s is not a file descriptor" fd))
    (let* ([st (aio-ensure-state! 'fd-poll-open)]
           [id (aio-next-id st)]
           [h (aio-poll-init (aio-state-loop st) fd id)])
      (when (= h 0)
        (raise (aio-io-condition 'fd-poll-open #f #f 'init-failed)))
      (let ([poll (make-aio-handle id h 'poll st (format "fd ~a" fd)
                    #f (make-aio-os-mutex) #f #f (make-aio-queue) #f #f
                    (make-aio-queue) #f)])
        (aio-register-handle! st poll)
        poll))))

(define %fd-poll-operation
  (lambda (poll events)
    (unless (and (aio-handle? poll) (eq? (aio-handle-kind poll) 'poll))
      ($oops 'fd-poll-operation "~s is not an fd poll handle" poll))
    (let ([bits (aio-poll-event-bits 'fd-poll-operation events)]
          [token (list 'fd-poll-operation)])
      (aio-make-operation
        (lambda (ss)
          (aio-check-handle-scope! 'fd-poll-operation poll)
          (with-aio-mutex (aio-handle-mutex poll)
            (cond
              [(aio-handle-closing? poll)
               (cons 'raise (aio-closed-condition 'fd-poll poll))]
              [(not (aio-queue-empty? (aio-handle-read-queue poll)))
               (cons 'raise
                 (aio-io-condition 'fd-poll poll (aio-handle-path poll)
                   'busy))]
              [else #f])))
        (lambda (ss deliver)
          (aio-check-handle-scope! 'fd-poll-operation poll)
          (let ([result
                 (with-aio-mutex (aio-handle-mutex poll)
                   (cond
                     [(aio-handle-closing? poll)
                      (cons 'immediate
                        (cons 'raise (aio-closed-condition 'fd-poll poll)))]
                     [(not (aio-queue-empty? (aio-handle-read-queue poll)))
                      (cons 'immediate
                        (cons 'raise
                          (aio-io-condition 'fd-poll poll
                            (aio-handle-path poll) 'busy)))]
                     [else
                      ($async-sync-slot-set! ss token
                        (aio-queue-push! (aio-handle-read-queue poll)
                          (cons ss deliver)))
                      (aio-handle-reading?-set! poll #t)
                      '(start)]))])
            (when (eq? (car result) 'start)
              (aio-run-on-owner! (aio-handle-state poll)
                (lambda ()
                  (let ([r (aio-poll-start (aio-handle-handle poll) bits)])
                    (when (fx< r 0)
                      (aio-on-poll (aio-handle-state poll)
                        (aio-handle-id poll) r 0))))))
            (if (eq? (car result) 'start)
                (list 'fd-poll (aio-handle-id poll))
                (begin (deliver (cdr result)) #f))))
        (lambda (vals) vals)
        (lambda (ss)
          (let ([node ($async-sync-slot-ref ss token #f)])
            (when node
              ($async-sync-slot-delete! ss token)
              (with-aio-mutex (aio-handle-mutex poll)
                (aio-queue-remove! (aio-handle-read-queue poll) node))))
          (let ([st (aio-handle-state poll)])
            (with-aio-mutex (aio-state-stop-mutex st)
              (aio-state-stop-set-set! st
                (cons poll (aio-state-stop-set st))))))))))

;;; ---------------------------------------------------------- processes

(define aio-string-has-nul?
  (lambda (s)
    (let loop ([i 0])
      (and (fx< i (string-length s))
           (or (char=? (string-ref s i) #\nul) (loop (fx+ i 1)))))))

(define aio-string-list-blob
  (lambda (who strings empty-ok?)
    (unless (and (list? strings) (for-all string? strings))
      ($oops who "~s is not a list of strings" strings))
    (when (and (null? strings) (not empty-ok?))
      ($oops who "argument list is empty"))
    (for-each
      (lambda (s)
        (when (aio-string-has-nul? s)
          ($oops who "string contains a nul character: ~s" s)))
      strings)
    (if (null? strings)
        (make-bytevector 1 0)
        (let* ([parts (map string->utf8 strings)]
               [length (fold-left
                         (lambda (n bv) (fx+ n (fx+ (bytevector-length bv) 1)))
                         0 parts)]
               [blob (make-bytevector length 0)])
          (let loop ([parts parts] [offset 0])
            (if (null? parts)
                blob
                (let ([n (bytevector-length (car parts))])
                  (bytevector-copy! (car parts) 0 blob offset n)
                  (loop (cdr parts) (fx+ offset (fx+ n 1))))))))))

(define aio-process-option
  (lambda (options key default)
    (let ([entry (assq key options)]) (if entry (cdr entry) default))))

(define aio-process-flags
  (lambda (who flags)
    (aio-symbols->flag-bits who flags "process flags" "a process flag"
      '((detached . 1) (windows-hide . 2)
        (windows-verbatim-arguments . 4)))))

(define aio-process-stdio
  (lambda (who value child-reads? inherit-fd)
    (cond
      [(eq? value 'pipe) (values (if child-reads? 2 3) -1 #t)]
      [(eq? value 'ignore) (values 0 -1 #f)]
      [(eq? value 'inherit) (values 1 inherit-fd #f)]
      [(and (fixnum? value) (fx>= value 0)) (values 1 value #f)]
      [else ($oops who "~s is not a stdio specification" value)])))

(define %process-spawn
  (case-lambda
    [(file arguments) (%process-spawn file arguments '())]
    [(file arguments options)
     (aio-check-path 'process-spawn file)
     (unless (list? options) ($oops 'process-spawn "~s is not an alist" options))
     (let* ([argv (aio-string-list-blob 'process-spawn
                    (cons file arguments) #f)]
            [environment (aio-process-option options 'environment #f)]
            [env-present? (not (eq? environment #f))]
            [env-strings
             (and env-present?
                  (map (lambda (entry)
                         (unless (and (pair? entry) (string? (car entry))
                                      (string? (cdr entry)))
                           ($oops 'process-spawn
                             "~s is not an environment entry" entry))
                         (string-append (car entry) "=" (cdr entry)))
                       environment))]
            [env (aio-string-list-blob 'process-spawn
                   (or env-strings '()) #t)]
            [cwd (aio-process-option options 'cwd "")]
            [flags (aio-process-flags 'process-spawn
                     (aio-process-option options 'flags '()))]
            [st (aio-ensure-state! 'process-spawn)]
            [process-id (aio-next-id st)])
       (unless (string? cwd) ($oops 'process-spawn "~s is not a cwd" cwd))
       (let-values ([(in-mode in-fd in-pipe?)
                     (aio-process-stdio 'process-spawn
                       (aio-process-option options 'stdin 'pipe) #t 0)]
                    [(out-mode out-fd out-pipe?)
                     (aio-process-stdio 'process-spawn
                       (aio-process-option options 'stdout 'pipe) #f 1)]
                    [(err-mode err-fd err-pipe?)
                     (aio-process-stdio 'process-spawn
                       (aio-process-option options 'stderr 'pipe) #f 2)])
         (let* ([in-id (and in-pipe? (aio-next-id st))]
                [out-id (and out-pipe? (aio-next-id st))]
                [err-id (and err-pipe? (aio-next-id st))]
                [in-h (if in-pipe? (aio-pipe-init (aio-state-loop st) in-id) 0)]
                [out-h (if out-pipe? (aio-pipe-init (aio-state-loop st) out-id) 0)]
                [err-h (if err-pipe? (aio-pipe-init (aio-state-loop st) err-id) 0)])
           (define (close-pipes)
             (when (and in-pipe? (not (= in-h 0))) (aio-handle-close in-h))
             (when (and out-pipe? (not (= out-h 0))) (aio-handle-close out-h))
             (when (and err-pipe? (not (= err-h 0))) (aio-handle-close err-h)))
           (when (or (and in-pipe? (= in-h 0))
                     (and out-pipe? (= out-h 0))
                     (and err-pipe? (= err-h 0)))
             (close-pipes)
             ($oops 'process-spawn "cannot allocate a stdio pipe"))
           (let ([ph (aio-process-spawn (aio-state-loop st) process-id file
                       argv (bytevector-length argv)
                       env (bytevector-length env) (if env-present? 1 0)
                       cwd flags
                       in-mode in-fd in-h out-mode out-fd out-h
                       err-mode err-fd err-h)])
             (if (fx< ph 0)
                 (begin
                   (close-pipes)
                   (raise (aio-io-condition 'process-spawn #f file ph)))
                 (let ([process (make-aio-handle process-id ph 'process st file
                                  #f (make-aio-os-mutex) #f #f (make-aio-queue) #f #f
                                  (make-aio-queue) #f)]
                       [stdin (and in-pipe?
                                (make-aio-handle in-id in-h 'pipe-stream st
                                  "process stdin" #f (make-aio-os-mutex)
                                  #f #f (make-aio-queue) #f #f
                                  (make-aio-queue) #f))]
                       [stdout (and out-pipe?
                                 (make-aio-handle out-id out-h 'pipe-stream st
                                   "process stdout" #f (make-aio-os-mutex)
                                   #f #f (make-aio-queue) #f #f
                                   (make-aio-queue) #f))]
                       [stderr (and err-pipe?
                                 (make-aio-handle err-id err-h 'pipe-stream st
                                   "process stderr" #f (make-aio-os-mutex)
                                   #f #f (make-aio-queue) #f #f
                                   (make-aio-queue) #f))])
                   (aio-register-handle! st process)
                   (when stdin (aio-register-handle! st stdin))
                   (when stdout (aio-register-handle! st stdout))
                   (when stderr (aio-register-handle! st stderr))
                   (values process stdin stdout stderr)))))))]))

(define %process-wait-operation
  (lambda (process)
    (unless (and (aio-handle? process)
                 (eq? (aio-handle-kind process) 'process))
      ($oops 'process-wait-operation "~s is not an async process" process))
    (let ([token (list 'process-wait-operation)])
      (aio-make-operation
      (lambda (ss)
        (aio-check-handle-scope! 'process-wait-operation process)
        (with-aio-mutex (aio-handle-mutex process)
          (let ([result (aio-handle-result process)])
            (cond
              [result (cons 'values (list (car result) (cdr result)))]
              [(aio-handle-closing? process)
               (cons 'raise (aio-closed-condition 'process-wait process))]
              [else #f]))))
      (lambda (ss deliver)
        (aio-check-handle-scope! 'process-wait-operation process)
        (let ([payload
               (with-aio-mutex (aio-handle-mutex process)
                 (let ([result (aio-handle-result process)])
                   (cond
                     [result
                      (cons 'values (list (car result) (cdr result)))]
                     [(aio-handle-closing? process)
                      (cons 'raise
                        (aio-closed-condition 'process-wait process))]
                     [else
                      ($async-sync-slot-set! ss token
                        (aio-queue-push! (aio-handle-accept-queue process)
                          (cons ss deliver)))
                      #f])))])
          (if payload
              (begin (deliver payload) #f)
              (list 'process-wait (aio-handle-id process)))))
      (lambda (vals) vals)
      (lambda (ss)
        (let ([node ($async-sync-slot-ref ss token #f)])
          (when node
            ($async-sync-slot-delete! ss token)
            (with-aio-mutex (aio-handle-mutex process)
              (aio-queue-remove!
                (aio-handle-accept-queue process) node)))))))))

;;; --------------------------------------- signal and filesystem watchers

(define aio-watch-open
  (lambda (who kind path init config)
    (let* ([st (aio-ensure-state! who)]
           [id (aio-next-id st)]
           [native (init (aio-state-loop st) id)])
      (when (= native 0) ($oops who "cannot allocate a native watcher"))
      (let ([watcher (make-aio-handle id native kind st path #f
                       (make-aio-os-mutex) #f #f (make-aio-queue) #f #f
                       (make-aio-queue) config)])
        (aio-register-handle! st watcher)
        watcher))))

(define aio-check-watcher
  (lambda (who watcher kind)
    (unless (and (aio-handle? watcher) (eq? (aio-handle-kind watcher) kind))
      ($oops who "~s is not an async ~a watcher" watcher kind))))

(define aio-watch-operation
  (lambda (who watcher kind start)
    (aio-check-watcher who watcher kind)
    (let ([token (list 'watch-operation)])
      (aio-make-operation
      (lambda (ss)
        (aio-check-handle-scope! who watcher)
        (with-aio-mutex (aio-handle-mutex watcher)
          (cond
            [(aio-handle-closing? watcher)
             (cons 'raise (aio-closed-condition who watcher))]
            [(not (aio-queue-empty? (aio-handle-read-queue watcher)))
             (cons 'raise
               (aio-io-condition who watcher (aio-handle-path watcher) 'busy))]
            [else #f])))
      (lambda (ss deliver)
        (aio-check-handle-scope! who watcher)
        (let ([result
               (with-aio-mutex (aio-handle-mutex watcher)
                 (cond
                   [(aio-handle-closing? watcher)
                    (cons 'immediate
                      (cons 'raise (aio-closed-condition who watcher)))]
                   [(not (aio-queue-empty? (aio-handle-read-queue watcher)))
                    (cons 'immediate
                      (cons 'raise
                        (aio-io-condition who watcher
                          (aio-handle-path watcher) 'busy)))]
                   [else
                    ($async-sync-slot-set! ss token
                      (aio-queue-push! (aio-handle-read-queue watcher)
                        (cons ss deliver)))
                    (aio-handle-reading?-set! watcher #t)
                    '(start)]))])
          (when (eq? (car result) 'start)
            (aio-run-on-owner! (aio-handle-state watcher)
              (lambda ()
                (let ([r (start)])
                  (when (fx< r 0)
                    (aio-on-watch (aio-handle-state watcher)
                      (aio-handle-id watcher)
                      (case kind
                        [(signal) AIO-EV-SIGNAL]
                        [(fs-event) AIO-EV-FS-EVENT]
                        [else AIO-EV-FS-POLL])
                      r 0))))))
          (if (eq? (car result) 'start)
              (list who (aio-handle-id watcher))
              (begin (deliver (cdr result)) #f))))
      (lambda (vals) vals)
      (lambda (ss)
        (let ([node ($async-sync-slot-ref ss token #f)])
          (when node
            ($async-sync-slot-delete! ss token)
            (with-aio-mutex (aio-handle-mutex watcher)
              (aio-queue-remove! (aio-handle-read-queue watcher) node))))
        (let ([st (aio-handle-state watcher)])
          (with-aio-mutex (aio-state-stop-mutex st)
            (aio-state-stop-set-set! st
              (cons watcher (aio-state-stop-set st))))))))))

(define %signal-open
  (lambda (signum)
    (unless (fixnum? signum) ($oops 'signal-open "~s is not a signal" signum))
    (aio-resolve-kernel!)
    (aio-watch-open 'signal-open 'signal (format "signal ~a" signum)
      aio-signal-init signum)))

(define %signal-receive-operation
  (lambda (watcher)
    (aio-check-watcher 'signal-receive-operation watcher 'signal)
    (aio-watch-operation 'signal-receive watcher 'signal
      (lambda ()
        (aio-signal-start (aio-handle-handle watcher)
          (aio-handle-result watcher) 1)))))

(define aio-fs-event-flag-bits
  (lambda (who flags)
    (aio-symbols->flag-bits who flags "filesystem event flags"
      "a filesystem event flag"
      '((watch-entry . 1) (stat . 2) (recursive . 4)))))

(define %fs-event-open
  (case-lambda
    [(path) (%fs-event-open path '())]
    [(path flags)
     (aio-check-path 'fs-event-open path)
     (aio-resolve-kernel!)
     (aio-watch-open 'fs-event-open 'fs-event path aio-fs-event-init
       (aio-fs-event-flag-bits 'fs-event-open flags))]))

(define %fs-event-receive-operation
  (lambda (watcher)
    (aio-check-watcher 'fs-event-receive-operation watcher 'fs-event)
    (aio-watch-operation 'fs-event-receive watcher 'fs-event
      (lambda ()
        (aio-fs-event-start (aio-handle-handle watcher)
          (aio-handle-path watcher) (aio-handle-result watcher))))))

(define %fs-poll-open
  (lambda (path interval)
    (aio-check-path 'fs-poll-open path)
    (unless (and (fixnum? interval) (fx> interval 0))
      ($oops 'fs-poll-open "~s is not a positive interval" interval))
    (aio-resolve-kernel!)
    (aio-watch-open 'fs-poll-open 'fs-poll path aio-fs-poll-init interval)))

(define %fs-poll-receive-operation
  (lambda (watcher)
    (aio-check-watcher 'fs-poll-receive-operation watcher 'fs-poll)
    (aio-watch-operation 'fs-poll-receive watcher 'fs-poll
      (lambda ()
        (aio-fs-poll-start (aio-handle-handle watcher)
          (aio-handle-path watcher) (aio-handle-result watcher))))))

;;; ----------------------------------------------------------------- tty

(define %tty-open
  (lambda (fd)
    (unless (and (fixnum? fd) (fx>= fd 0))
      ($oops 'tty-open "~s is not a file descriptor" fd))
    (let* ([st (aio-ensure-state! 'tty-open)]
           [id (aio-next-id st)]
           [native (aio-tty-init (aio-state-loop st) fd id)])
      (when (= native 0)
        (raise (aio-io-condition 'tty-open #f (format "fd ~a" fd)
                 'init-failed)))
      (let ([tty (make-aio-handle id native 'tty-stream st
                   (format "tty fd ~a" fd) #f (make-aio-os-mutex)
                   #f #f (make-aio-queue) #f #f (make-aio-queue) #f)])
        (aio-register-handle! st tty)
        tty))))

;;; ----------------------------------------------------- system snapshots

(define aio-system-cstring
  (lambda (who proc field)
    (aio-resolve-kernel!)
    (let ([buf (make-bytevector 4096)])
      (let ([r (proc field buf 4096)])
        (if (fx< r 0)
            (raise (aio-io-condition who #f #f r))
            (bv->cstring buf))))))

(define %system-cpu-info
  (lambda ()
    (aio-resolve-kernel!)
    (let ([ctx (aio-cpu-info)])
      (when (= ctx 0)
        (raise (aio-io-condition 'system-cpu-info #f #f 'unavailable)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([count (aio-cpu-info-count ctx)] [buf (make-bytevector 1024)])
            (let loop ([i 0] [items '()])
              (if (fx= i count)
                  (reverse items)
                  (let ([r (aio-cpu-info-model ctx i buf 1024)])
                    (when (fx< r 0)
                      (raise (aio-io-condition 'system-cpu-info #f #f r)))
                    (loop (fx+ i 1)
                      (cons
                        (list (cons 'model (bv->cstring buf))
                              (cons 'speed (aio-cpu-info-field ctx i 0))
                              (cons 'user (aio-cpu-info-field ctx i 1))
                              (cons 'nice (aio-cpu-info-field ctx i 2))
                              (cons 'system (aio-cpu-info-field ctx i 3))
                              (cons 'idle (aio-cpu-info-field ctx i 4))
                              (cons 'irq (aio-cpu-info-field ctx i 5)))
                        items)))))))
        (lambda () (aio-cpu-info-free ctx))))))

(define %system-interface-info
  (lambda ()
    (aio-resolve-kernel!)
    (let ([ctx (aio-interface-info)])
      (when (= ctx 0)
        (raise (aio-io-condition 'system-interface-info #f #f 'unavailable)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let ([count (aio-interface-count ctx)]
                [name (make-bytevector 1024)]
                [address (make-bytevector 64)]
                [netmask (make-bytevector 64)])
            (let loop ([i 0] [items '()])
              (if (fx= i count)
                  (reverse items)
                  (let ([nr (aio-interface-name ctx i name 1024)]
                        [ar (aio-interface-address ctx i 0 address 64)]
                        [mr (aio-interface-address ctx i 1 netmask 64)]
                        [physical (make-bytevector 6)])
                    (when (or (fx< nr 0) (fx< ar 0) (fx< mr 0))
                      (raise (aio-io-condition 'system-interface-info #f #f
                               (cond [(fx< nr 0) nr] [(fx< ar 0) ar] [else mr]))))
                    (aio-interface-physical ctx i physical)
                    (loop (fx+ i 1)
                      (cons
                        (list (cons 'name (bv->cstring name))
                              (cons 'address (bv->cstring address))
                              (cons 'netmask (bv->cstring netmask))
                              (cons 'family (quotient ar 65536))
                              (cons 'internal? (not (fx= 0
                                (aio-interface-internal ctx i))))
                              (cons 'physical-address physical))
                        items)))))))
        (lambda () (aio-interface-free ctx))))))

(define %system-resource-usage
  (lambda ()
    (aio-resolve-kernel!)
    (let ([ctx (aio-rusage)])
      (when (= ctx 0)
        (raise (aio-io-condition 'system-resource-usage #f #f 'unavailable)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (define (field i) (aio-rusage-field ctx i))
          (list (cons 'user-time (cons (field 0) (field 1)))
                (cons 'system-time (cons (field 2) (field 3)))
                (cons 'maximum-resident-set-size (field 4))
                (cons 'minor-page-faults (field 8))
                (cons 'major-page-faults (field 9))
                (cons 'swaps (field 10))
                (cons 'input-blocks (field 11))
                (cons 'output-blocks (field 12))
                (cons 'signals (field 15))
                (cons 'voluntary-context-switches (field 16))
                (cons 'involuntary-context-switches (field 17))))
        (lambda () (aio-rusage-free ctx))))))

(define %async-loop-metrics
  (lambda ()
    (let ([st (aio-ensure-state! 'async-loop-metrics)])
      (define (field i) (aio-loop-metric (aio-state-loop st) i))
      (aio-debug-check-owner! st)
      (list (cons 'now (field 0))
            (cons 'idle-time (field 1))
            (cons 'backend-timeout (field 2))
            (cons 'backend-fd (field 3))
            (cons 'alive? (not (= (field 4) 0)))
            (cons 'loop-count (field 5))
            (cons 'events (field 6))
            (cons 'events-waiting (field 7))))))
