;;; asyncio.ss
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

;;; libuv-backed asynchronous I/O for the fiber scheduler (ASYNC.md layer 4).
;;;
;;; The C shim (c/asyncio.c) owns the concrete layouts of uv_loop_t,
;;; uv_handle_t, and uv_req_t.  Scheme code refers to native objects through
;;; sealed records and stable integer identifiers kept in a per-scheduler
;;; registry.  Native completions are enqueued by a single locked trampoline
;;; and dispatched on the scheduler thread by the poll hook; a libuv worker
;;; callback never runs Scheme code, and the trampoline never raises.

(let ()  ; private scope: public names are assigned to their declared globals

(define-syntax aio-trace (syntax-rules () [(_ e ...) (void)]))

;;; ------------------------------------------------------------ constants

;;; event kinds reported by the shim (see c/asyncio.c)
(define AIO-EV-ACCEPT 1)
(define AIO-EV-READ 2)
(define AIO-EV-WRITE 3)
(define AIO-EV-CONNECT 4)
(define AIO-EV-SHUTDOWN 5)
(define AIO-EV-FS 6)
(define AIO-EV-DNS 7)
(define AIO-EV-CLOSE 8)

;;; per-poll dispatch bound so a sustained completion stream cannot starve
;;; runnable tasks
(define aio-dispatch-bound 256)

;;; ------------------------------------------------------------ conditions

(define-condition-type &async-io &error
  make-async-io-condition% %async-io-condition?
  (operation %async-io-condition-operation)
  (handle %async-io-condition-handle)
  (path %async-io-condition-path)
  (code %async-io-condition-code))

;;; ------------------------------------------------------- shim loading

(define aio-loaded? #f)

;;; foreign-procedure bindings, resolved lazily from the statically linked
;;; kernel
(define aio-loop-open #f)
(define aio-set-notify #f)
(define aio-loop-run #f)
(define aio-loop-alive #f)
(define aio-loop-destroy #f)
(define aio-wakeup-init #f)
(define aio-wakeup-send #f)
(define aio-bridge-init #f)
(define aio-bridge-start #f)
(define aio-bridge-stop #f)
(define aio-handle-close #f)
(define aio-handle-is-closing #f)
(define aio-tcp-init #f)
(define aio-tcp-bind #f)
(define aio-listen-start #f)
(define aio-accept #f)
(define aio-tcp-connect #f)
(define aio-pipe-init #f)
(define aio-pipe-bind #f)
(define aio-pipe-connect #f)
(define aio-read-start #f)
(define aio-read-stop #f)
(define aio-read-copy #f)
(define aio-free #f)
(define aio-write #f)
(define aio-shutdown #f)
(define aio-dns-lookup #f)
(define aio-dns-cancel #f)
(define aio-dns-count #f)
(define aio-dns-addr #f)
(define aio-dns-free #f)
(define aio-fs-open #f)
(define aio-fs-read #f)
(define aio-fs-write #f)
(define aio-fs-close-fd #f)
(define aio-fs-close-now #f)
(define aio-fs-stat #f)
(define aio-fs-fstat #f)
(define aio-fs-unlink #f)
(define aio-fs-rename #f)
(define aio-fs-mkdir #f)
(define aio-fs-rmdir #f)
(define aio-fs-req-free #f)
(define aio-fs-cancel #f)
(define aio-fs-data #f)
(define aio-fs-buf-free #f)
(define aio-fs-stat-field #f)
(define aio-strerror-into #f)
(define aio-err-name-into #f)
(define aio-eof-code #f)
(define aio-eagain-code #f)

(define aio-resolve!
  (lambda ()
    (set! aio-loop-open (foreign-procedure "aio_loop_open" () void*))
    (set! aio-set-notify (foreign-procedure "aio_set_notify" (void* void*) void))
    (set! aio-loop-run (foreign-procedure "aio_loop_run" (void* int) int))
    (set! aio-loop-alive (foreign-procedure "aio_loop_alive" (void*) int))
    (set! aio-loop-destroy (foreign-procedure "aio_loop_destroy" (void*) int))
    (set! aio-wakeup-init (foreign-procedure "aio_wakeup_init" (void*) void*))
    (set! aio-wakeup-send (foreign-procedure "aio_wakeup_send" (void*) int))
    (set! aio-bridge-init (foreign-procedure "aio_bridge_init" (void*) void*))
    (set! aio-bridge-start (foreign-procedure "aio_bridge_start" (void* integer-64) int))
    (set! aio-bridge-stop (foreign-procedure "aio_bridge_stop" (void*) int))
    (set! aio-handle-close (foreign-procedure "aio_handle_close" (void*) void))
    (set! aio-handle-is-closing (foreign-procedure "aio_handle_is_closing" (void*) int))
    (set! aio-tcp-init (foreign-procedure "aio_tcp_init" (void* integer-64) void*))
    (set! aio-tcp-bind (foreign-procedure "aio_tcp_bind" (void* string integer-64) int))
    (set! aio-listen-start (foreign-procedure "aio_listen_start" (void* integer-64) int))
    (set! aio-accept (foreign-procedure "aio_accept" (void* void*) int))
    (set! aio-tcp-connect (foreign-procedure "aio_tcp_connect" (void* string integer-64 integer-64) int))
    (set! aio-pipe-init (foreign-procedure "aio_pipe_init" (void* integer-64) void*))
    (set! aio-pipe-bind (foreign-procedure "aio_pipe_bind" (void* string) int))
    (set! aio-pipe-connect (foreign-procedure "aio_pipe_connect" (void* string integer-64) int))
    (set! aio-read-start (foreign-procedure "aio_read_start" (void*) int))
    (set! aio-read-stop (foreign-procedure "aio_read_stop" (void*) int))
    (set! aio-read-copy (foreign-procedure "aio_read_copy" (void* u8* integer-64) void))
    (set! aio-free (foreign-procedure "aio_free" (void*) void))
    (set! aio-write (foreign-procedure "aio_write" (void* u8* integer-64 integer-64) int))
    (set! aio-shutdown (foreign-procedure "aio_shutdown" (void* integer-64) int))
    (set! aio-dns-lookup (foreign-procedure "aio_dns_lookup" (void* string string integer-64) integer-64))
    (set! aio-dns-cancel (foreign-procedure "aio_dns_cancel" (void*) int))
    (set! aio-dns-count (foreign-procedure "aio_dns_count" (void*) integer-64))
    (set! aio-dns-addr (foreign-procedure "aio_dns_addr" (void* integer-64 u8* integer-64) integer-64))
    (set! aio-dns-free (foreign-procedure "aio_dns_free" (void*) void))
    (set! aio-fs-open (foreign-procedure "aio_fs_open" (void* string int integer-64 integer-64) integer-64))
    (set! aio-fs-read (foreign-procedure "aio_fs_read" (void* integer-64 integer-64 integer-64 integer-64) integer-64))
    (set! aio-fs-write (foreign-procedure "aio_fs_write" (void* integer-64 u8* integer-64 integer-64 integer-64) integer-64))
    (set! aio-fs-close-fd (foreign-procedure "aio_fs_close_fd" (void* integer-64 integer-64) integer-64))
    (set! aio-fs-close-now (foreign-procedure "aio_fs_close_now" (integer-64) int))
    (set! aio-fs-stat (foreign-procedure "aio_fs_stat" (void* string integer-64) integer-64))
    (set! aio-fs-fstat (foreign-procedure "aio_fs_fstat" (void* integer-64 integer-64) integer-64))
    (set! aio-fs-unlink (foreign-procedure "aio_fs_unlink" (void* string integer-64) integer-64))
    (set! aio-fs-rename (foreign-procedure "aio_fs_rename" (void* string string integer-64) integer-64))
    (set! aio-fs-mkdir (foreign-procedure "aio_fs_mkdir" (void* string integer-64 integer-64) integer-64))
    (set! aio-fs-rmdir (foreign-procedure "aio_fs_rmdir" (void* string integer-64) integer-64))
    (set! aio-fs-req-free (foreign-procedure "aio_fs_req_free" (void*) void))
    (set! aio-fs-cancel (foreign-procedure "aio_fs_cancel" (void*) int))
    (set! aio-fs-data (foreign-procedure "aio_fs_data" (void*) void*))
    (set! aio-fs-buf-free (foreign-procedure "aio_fs_buf_free" (void*) void))
    (set! aio-fs-stat-field (foreign-procedure "aio_fs_stat_field" (void* integer-64) integer-64))
    (set! aio-strerror-into (foreign-procedure "aio_strerror_into" (integer-64 u8* integer-64) void))
    (set! aio-err-name-into (foreign-procedure "aio_err_name_into" (integer-64 u8* integer-64) void))
    (set! aio-eof-code (foreign-procedure "aio_eof_code" () integer-64))
    (set! aio-eagain-code (foreign-procedure "aio_eagain_code" () integer-64))))

(define aio-resolve-kernel!
  (lambda ()
    (unless aio-loaded?
      (aio-resolve!)
      (set! aio-loaded? #t))))

;;; ------------------------------------------------------------ records

;;; One per scheduler that has touched I/O.  Requests are in-flight native
;;; operations keyed by request id; handles are live listeners/streams keyed
;;; by handle id.  The tables are guarded because a cancellation nack can run
;;; on a thread other than the scheduler's.
(define-record-type (aio-state make-aio-state aio-state?)
  (nongenerative)
  (sealed #t)
  (fields
    (immutable loop)              ; void* aio_loop_t
    (immutable wakeup)            ; void* uv_async_t
    (immutable bridge)            ; void* uv_timer_t
    (mutable next-id)
    (immutable requests)          ; id -> aio-req
    (immutable requests-mutex)
    (immutable handles)           ; id -> weak-cons wrapper #t
    (immutable files)             ; fd -> weak-cons async-file #t
    (mutable completions)         ; list of (id kind status aux)
    (mutable commands)            ; owner-thread thunks, newest first
    (immutable command-mutex)     ; also guards closing and wakeup lifetime
    (mutable stop-set)            ; streams that may need uv_read_stop
    (immutable stop-mutex)
    (mutable closing?)
    (immutable guardian)
    (immutable file-guardian)))

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
    (mutable read-queue)          ; list of (ss . deliver)
    (mutable reading?)
    (mutable eof?)
    (mutable accept-queue)        ; list of (ss . deliver)
    (mutable affinities)))        ; list of (task . release)

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
    (mutable queue)
    (mutable affinities)))        ; list of (task . release)

;;; ------------------------------------------------- notify trampoline

;;; maps aio_loop_t pointers to their io state
(define aio-loop-registry (make-eq-hashtable))
(define aio-loop-registry-mutex (make-mutex))

;;; Runs inside uv_run on the scheduler thread.  Enqueues the completion and
;;; nothing more; a condition is swallowed rather than escaping through C.
(define aio-notify-trampoline
  (let ([p (foreign-callable
             (lambda (lp id kind status aux)
               (guard (c [else (void)])
                 (let ([st
                        (with-mutex aio-loop-registry-mutex
                          (hashtable-ref aio-loop-registry lp #f))])
                   (when st
                     (aio-state-completions-set! st
                       (cons (list id kind status aux)
                             (aio-state-completions st)))))))
             (void* integer-64 integer-64 integer-64 void*)
             void)])
    (lock-object p)
    p))

;;; ------------------------------------------------------------ helpers

(define aio-next-id
  (lambda (st)
    (let ([id (aio-state-next-id st)])
      (aio-state-next-id-set! st (fx+ id 1))
      id)))

(define aio-waiter-dead?
  (lambda (ss) (not ($async-sync-state-live? ss))))

(define aio-io-condition
  (lambda (operation handle path code)
    (make-async-io-condition% operation handle path code)))

(define aio-closed-condition
  (lambda (operation handle)
    (make-async-io-condition% operation handle (aio-handle-path handle) 'closed)))

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

(define aio-register-handle!
  (lambda (st w)
    (hashtable-set! (aio-state-handles st) (aio-handle-id w) (weak-cons w #t))
    ((aio-state-guardian st) w)))

(define aio-register-file!
  (lambda (st f)
    (hashtable-set! (aio-state-files st) (async-file-fd f) (weak-cons f #t))
    ((aio-state-file-guardian st) f)
    f))

(define aio-unregister-file!
  (lambda (f)
    (hashtable-delete! (aio-state-files (async-file-state f))
      (async-file-fd f))))

(define aio-lookup-handle
  (lambda (st id)
    (let ([p (hashtable-ref (aio-state-handles st) id #f)])
      (and p
           (let ([w (car p)])
             (and (not (bwp-object? w)) w))))))

;;; A scheduler-local resource pins each task that uses it.  Closing the
;;; resource releases every such task, allowing later scheduler migration.
(define aio-bind-current-task-to-handle!
  (lambda (h)
    (let ([task (current-async-task)])
      (when task
        (with-mutex (aio-handle-mutex h)
          (unless (assq task (aio-handle-affinities h))
            (aio-handle-affinities-set! h
              (cons (cons task ($async-pin-current-task!))
                    (aio-handle-affinities h)))))))))

(define aio-release-handle-affinities!
  (lambda (h)
    (let ([affinities
           (with-mutex (aio-handle-mutex h)
             (let ([affinities (aio-handle-affinities h)])
               (aio-handle-affinities-set! h '())
               affinities))])
      (for-each (lambda (entry) ((cdr entry))) affinities))))

(define aio-bind-current-task-to-file!
  (lambda (f)
    (let ([task (current-async-task)])
      (when task
        (with-mutex (async-file-mutex f)
          (unless (assq task (async-file-affinities f))
            (async-file-affinities-set! f
              (cons (cons task ($async-pin-current-task!))
                    (async-file-affinities f)))))))))

(define aio-release-file-affinities!
  (lambda (f)
    (let ([affinities
           (with-mutex (async-file-mutex f)
             (let ([affinities (async-file-affinities f)])
               (async-file-affinities-set! f '())
               affinities))])
      (for-each (lambda (entry) ((cdr entry))) affinities))))

;;; register a request; finish runs exactly once at dispatch, canceled or
;;; not, and returns a payload to deliver or #f to stay silent
(define aio-register-request!
  (lambda (st id req)
    (with-mutex (aio-state-requests-mutex st)
      (hashtable-set! (aio-state-requests st) id req))))

;;; Native libuv objects are touched only by their loop owner.  Foreign
;;; threads enqueue identities or Scheme data and use uv_async_send solely as
;;; the wakeup mechanism.
(define aio-submit-command!
  (lambda (st command)
    (let ([accepted?
           (with-mutex (aio-state-command-mutex st)
             (if (aio-state-closing? st)
                 #f
                 (begin
                   (aio-state-commands-set! st
                     (cons command (aio-state-commands st)))
                   #t)))])
      (when accepted?
        (aio-wakeup-send (aio-state-loop st)))
      accepted?)))

(define aio-drain-commands!
  (lambda (st)
    (let ([commands
           (with-mutex (aio-state-command-mutex st)
             (let ([commands (reverse (aio-state-commands st))])
               (aio-state-commands-set! st '())
               commands))])
      (for-each (lambda (command) (command)) commands))))

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
           (with-mutex (aio-state-requests-mutex st)
             (let ([req (hashtable-ref (aio-state-requests st) id #f)])
               (when req (aio-req-canceled?-set! req #t))
               (and req #t)))])
      (when found?
        (aio-submit-command! st
          (lambda ()
            ;; Completion and this command are both dispatched by the loop
            ;; owner, so cancel-data cannot be freed between lookup and use.
            (let ([req
                   (with-mutex (aio-state-requests-mutex st)
                     (hashtable-ref (aio-state-requests st) id #f))])
              (when req
                (let ([cd (aio-req-cancel-data req)])
                  (when cd
                    (case (aio-req-kind req)
                      [(fs) (aio-fs-cancel cd)]
                      [(dns) (aio-dns-cancel cd)]
                      [else (void)])))))))))))

;;; ------------------------------------------------------------- dispatch

(define aio-dispatch-event
  (lambda (st id kind status aux)
    (cond
      [(fx= kind AIO-EV-READ) (aio-on-read st id status aux)]
      [(fx= kind AIO-EV-ACCEPT) (aio-on-accept st id status)]
      [(fx= kind AIO-EV-CLOSE) (aio-on-close st id)]
      [else (aio-on-request st id status aux)])))

(define aio-on-request
  (lambda (st id status aux)
    (let ([req
           (with-mutex (aio-state-requests-mutex st)
             (let ([req (hashtable-ref (aio-state-requests st) id #f)])
               (when req (hashtable-delete! (aio-state-requests st) id))
               req))])
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
           (with-mutex (aio-handle-mutex h)
             (aio-handle-reading?-set! h #f)
             (aio-handle-read-queue-set! h
               (filter (lambda (w) (not (aio-waiter-dead? (car w))))
                 (aio-handle-read-queue h)))
             (let ([q (aio-handle-read-queue h)])
               (cond
                 [(or (aio-state-closing? st) (aio-handle-closing? h))
                  (set! free-aux? (fx> status 0))]
                 [(fx= status (aio-eof-code))
                  (aio-handle-eof?-set! h #t)
                  (aio-handle-read-queue-set! h '())
                  (set! deliveries
                    (map (lambda (w)
                           (cons (cdr w) (cons 'values (list #!eof))))
                      q))]
                 [(null? q)
                  (set! free-aux? (fx> status 0))]
                 [else
                  (aio-handle-read-queue-set! h (cdr q))
                  (set! deliveries
                    (list (cons (cdar q)
                            (aio-read-payload h status aux))))]))
             ;; arm the next reader, if any
           (when (and (not (aio-handle-reading? h))
                      (not (aio-handle-closing? h))
                      (not (aio-handle-eof? h))
                      (pair? (aio-handle-read-queue h)))
             (aio-handle-reading?-set! h #t)
               (aio-read-start (aio-handle-handle h))))
           (when free-aux? (aio-free aux))
           (for-each (lambda (d) ((car d) (cdr d))) deliveries))]))))

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
                          st #f #f (make-mutex) #f #f '() #f #f '() '())])
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
          (with-mutex (aio-handle-mutex h)
            (aio-handle-accept-queue-set! h
              (filter (lambda (w) (not (aio-waiter-dead? (car w))))
                (aio-handle-accept-queue h)))
            (let ([q (aio-handle-accept-queue h)])
              (when (pair? q)
                (let ([payload
                       (if (fx< status 0)
                           (cons 'raise
                             (aio-io-condition 'accept h
                               (aio-handle-path h) status))
                           (aio-attempt-accept st h))])
                  (when payload
                    (aio-handle-accept-queue-set! h (cdr q))
                    (set! delivery (cons (cdar q) payload)))))))
          (when delivery ((car delivery) (cdr delivery))))))))

(define aio-on-close
  (lambda (st id)
    (let ([h (aio-lookup-handle st id)])
      (when h (aio-handle-closed?-set! h #t))
      (hashtable-delete! (aio-state-handles st) id))))

(define aio-drain-completions!
  (lambda (st)
    (let* ([ordered (reverse (aio-state-completions st))]
           [dispatch
             (let take ([xs ordered] [n aio-dispatch-bound])
               (if (or (null? xs) (fx= n 0))
                   '()
                   (cons (car xs) (take (cdr xs) (fx- n 1)))))]
           [remaining (list-tail ordered (length dispatch))])
      ;; The state stores newest first so the notify trampoline can cons.
      (aio-state-completions-set! st (reverse remaining))
      (for-each
        (lambda (ev)
          (aio-dispatch-event st (car ev) (cadr ev) (caddr ev) (cadddr ev)))
        dispatch))))

;;; deferred uv_read_stop requests: set by cancellation nacks that may run
;;; off the scheduler thread, drained by the poll hook on the scheduler thread
(define aio-drain-stop-set!
  (lambda (st)
    (let ([hs (with-mutex (aio-state-stop-mutex st)
                (let ([hs (aio-state-stop-set st)])
                  (aio-state-stop-set-set! st '())
                  hs))])
      (for-each
        (lambda (h)
          (with-mutex (aio-handle-mutex h)
            (when (and (aio-handle-reading? h)
                       (null? (aio-handle-read-queue h)))
              (aio-handle-reading?-set! h #f)
              (aio-read-stop (aio-handle-handle h)))))
        hs))))

(define aio-drain-guardian!
  (lambda (st)
    (let ([g (aio-state-guardian st)])
      (let loop ()
        (let ([w (g)])
          (when w
            (aio-close-handle w 'finalized)
            (loop)))))))

(define aio-finalize-file!
  (lambda (f)
    (let ([close?
           (with-mutex (async-file-mutex f)
             (if (async-file-closed? f)
                 #f
                 (begin
                   (async-file-closed?-set! f #t)
                   #t)))])
      (when close?
        (aio-fs-close-now (async-file-fd f))
        (aio-release-file-affinities! f))
      (aio-unregister-file! f))))

(define aio-drain-file-guardian!
  (lambda (st)
    (let ([g (aio-state-file-guardian st)])
      (let loop ()
        (let ([f (g)])
          (when f
            (aio-finalize-file! f)
            (loop)))))))

;;; ------------------------------------------------------- poll and wake

(define aio-arm-bridge
  (lambda (st sched)
    (let ([ts ($async-scheduler-timers sched)])
      (when (pair? ts)
        (let* ([deadline ($async-timer-deadline (car ts))]
               [delta (max 0 (- deadline ($async-monotonic-us)))])
          (aio-bridge-start (aio-state-loop st)
            (quotient (+ delta 999) 1000)))))))

(define aio-poll
  (lambda (sched block?)
    (let ([st ($async-scheduler-io-state sched)])
      (when (and st (not (aio-state-closing? st)))
        (aio-drain-guardian! st)
        (aio-drain-file-guardian! st)
        (aio-drain-commands! st)
        (aio-drain-stop-set! st)
        (when block? (aio-arm-bridge st sched))
        (aio-loop-run (aio-state-loop st) (if block? 1 0))
        (when block? (aio-bridge-stop (aio-state-loop st)))
        (aio-drain-completions! st)))))

;;; ------------------------------------------------------------ shutdown

;;; Orderly native shutdown (ASYNC.md "Resource finalization"): close every
;;; handle, cancel in-flight requests, run the loop until the close and
;;; cancellation callbacks complete, then release the loop.  Tasks are
;;; already terminal when this runs, so completions only reclaim native
;;; resources.
(define aio-io-shutdown
  (lambda (sched)
    (let ([st ($async-scheduler-io-state sched)])
      (when (and st (not (aio-state-closing? st)))
        (with-mutex (aio-state-command-mutex st)
          (aio-state-closing?-set! st #t))
        (aio-drain-commands! st)
        (let-values ([(ks vs) (hashtable-entries (aio-state-handles st))])
          (vector-for-each
            (lambda (p)
              (let ([w (car p)])
                (unless (bwp-object? w)
                  (aio-handle-closing?-set! w #t)
                  (unless (fx= 1 (aio-handle-is-closing (aio-handle-handle w)))
                    (aio-handle-close (aio-handle-handle w))))))
            vs))
        (with-mutex (aio-state-requests-mutex st)
          (let-values ([(ks vs) (hashtable-entries (aio-state-requests st))])
            (vector-for-each
              (lambda (req)
                (aio-req-canceled?-set! req #t)
                (let ([cd (aio-req-cancel-data req)])
                  (when cd
                    (case (aio-req-kind req)
                      [(fs) (aio-fs-cancel cd)]
                      [(dns) (aio-dns-cancel cd)]
                      [else (void)]))))
              vs)))
        (let-values ([(ks vs) (hashtable-entries (aio-state-handles st))])
          (vector-for-each
            (lambda (p)
              (let ([w (car p)])
                (unless (bwp-object? w)
                  (aio-release-handle-affinities! w))))
            vs))
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
          (when (pair? (aio-state-completions st))
            (aio-drain-completions! st)
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
        (aio-loop-destroy (aio-state-loop st))
        (with-mutex aio-loop-registry-mutex
          (hashtable-delete! aio-loop-registry (aio-state-loop st)))))))

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
                  (aio-loop-destroy loop)
                  ($oops who "cannot initialize the libuv timer handle"))
                (let ([st (make-aio-state loop wakeup bridge
                        1 (make-eq-hashtable) (make-mutex)
                        (make-eq-hashtable) (make-eq-hashtable)
                        '() '() (make-mutex)
                        '() (make-mutex) #f
                        (make-guardian) (make-guardian))])
                  (with-mutex aio-loop-registry-mutex
                    (hashtable-set! aio-loop-registry loop st))
                  (aio-set-notify loop (foreign-callable-entry-point aio-notify-trampoline))
                  ($async-scheduler-io-state-set! sched st)
                  ($async-scheduler-poll-proc-set! sched aio-poll)
                  ($async-scheduler-wake-proc-set! sched
                    (lambda ()
                      (with-mutex (aio-state-command-mutex st)
                        (unless (aio-state-closing? st)
                          (aio-wakeup-send loop)))))
                  st))))))))

;;; ------------------------------------------------------------ handles

;;; owner-thread close: wakes pending readers/writers/acceptors with a
;;; closed-handle condition, then closes the native handle
(define aio-cancel-handle-requests!
  (lambda (w operation)
    (let ([deliveries '()])
      (with-mutex (aio-state-requests-mutex (aio-handle-state w))
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
    (let-values ([(close? waiters)
                  (with-mutex (aio-handle-mutex w)
                    (if (aio-handle-closing? w)
                        (values #f '())
                        (begin
                          (aio-handle-closing?-set! w #t)
                          (let ([waiters
                                 (append (aio-handle-read-queue w)
                                         (aio-handle-accept-queue w))])
                            (aio-handle-read-queue-set! w '())
                            (aio-handle-accept-queue-set! w '())
                            (values #t waiters)))))])
      (when close?
        (aio-release-handle-affinities! w)
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
    (when (aio-handle-port-owned? s)
      ($oops who "async stream ownership has been transferred to a port"))))

(define aio-check-handle-owner!
  (lambda (who h)
    (let ([sched (current-async-scheduler)])
      (unless (and sched
                   (eq? ($async-scheduler-io-state sched)
                        (aio-handle-state h)))
        ($oops who "async handle belongs to another scheduler"))
      (aio-bind-current-task-to-handle! h))))

(define aio-check-stream-access!
  (lambda (who s allow-owned?)
    (aio-check-stream who s)
    (aio-check-handle-owner! who s)
    (unless allow-owned?
      (with-mutex (aio-handle-mutex s)
        (when (aio-handle-port-owned? s)
          ($oops who "async stream ownership has been transferred to a port"))))))

(define aio-claim-stream-for-port!
  (lambda (who s)
    (aio-check-stream who s)
    (aio-check-handle-owner! who s)
    (with-mutex (aio-handle-mutex s)
      (when (aio-handle-port-owned? s)
        ($oops who "async stream ownership has already been transferred to a port"))
      (when (aio-handle-closing? s)
        (raise (aio-closed-condition who s)))
      (aio-handle-port-owned?-set! s #t))))

(define aio-close-owned-handle
  (lambda (who h)
    (aio-check-handle-owner! who h)
    (aio-close-handle h 'close)))

(define %stream-read-operation
  (lambda (s . allow-owned-option)
    (aio-check-stream 'stream-read-operation s)
    (let ([allow-owned? (and (pair? allow-owned-option)
                             (car allow-owned-option))])
      (make-operation
        (lambda (ss)
          (aio-check-stream-access! 'stream-read-operation s allow-owned?)
        (with-mutex (aio-handle-mutex s)
          (cond
            [(aio-handle-eof? s) (cons 'values (list #!eof))]
            [(aio-handle-closing? s)
             (cons 'raise (aio-closed-condition 'read s))]
            [else #f])))
        (lambda (ss deliver)
          (aio-check-stream-access! 'stream-read-operation s allow-owned?)
        (with-mutex (aio-handle-mutex s)
          (cond
            [(aio-handle-eof? s)
             (deliver (cons 'values (list #!eof)))
             #f]
            [(aio-handle-closing? s)
             (deliver (cons 'raise (aio-closed-condition 'read s)))
             #f]
            [else
             (aio-handle-read-queue-set! s
               (append (aio-handle-read-queue s) (list (cons ss deliver))))
             (unless (aio-handle-reading? s)
               (aio-handle-reading?-set! s #t)
               (aio-read-start (aio-handle-handle s)))
             (list 'read (aio-handle-id s))])))
        (lambda (vals) vals)
        (lambda (ss)
        (with-mutex (aio-handle-mutex s)
          (aio-handle-read-queue-set! s
            (let loop ([q (aio-handle-read-queue s)])
              (cond
                [(null? q) '()]
                [(eq? (caar q) ss) (cdr q)]
                [else (cons (car q) (loop (cdr q)))]))))
        (let ([st (aio-handle-state s)])
          (with-mutex (aio-state-stop-mutex st)
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
      (make-operation
        (lambda (ss)
          (aio-check-stream-access! 'stream-write-operation s allow-owned?)
          (and (aio-handle-closing? s)
               (cons 'raise (aio-closed-condition 'write s))))
        (lambda (ss deliver)
          (aio-check-stream-access! 'stream-write-operation s allow-owned?)
          (if (aio-handle-closing? s)
              (begin
                (deliver (cons 'raise (aio-closed-condition 'write s)))
                #f)
              (let* ([st (aio-handle-state s)]
                     [id (aio-next-id st)]
                     [len (bytevector-length bv)]
                     [r (aio-write (aio-handle-handle s) bv len id)])
                ($async-sync-slot-set! ss token (cons st id))
                (if (< r 0)
                    (begin
                      (deliver
                        (cons 'raise
                          (aio-io-condition 'write s (aio-handle-path s) r)))
                      #f)
                    (begin
                      (aio-register-request! st id
                        (make-aio-req 'write s deliver #f
                          (aio-plain-finish
                            (lambda (status aux)
                              (if (fx= status 0)
                                  (cons 'values (list len))
                                  (cons 'raise
                                    (aio-io-condition 'write s (aio-handle-path s) status)))))
                          #f))
                      (list 'write id))))))
        (lambda (vals) vals)
        (aio-request-nack token)))))

(define stream-shutdown-operation
  (lambda (s)
    (aio-check-stream 'stream-shutdown s)
    (let ([token (list 'stream-shutdown-operation)])
      (make-operation
        (lambda (ss)
          (aio-check-stream-access! 'stream-shutdown s #f)
          (and (aio-handle-closing? s)
               (cons 'raise (aio-closed-condition 'shutdown s))))
        (lambda (ss deliver)
          (aio-check-stream-access! 'stream-shutdown s #f)
          (if (aio-handle-closing? s)
              (begin
                (deliver (cons 'raise (aio-closed-condition 'shutdown s)))
                #f)
              (let* ([st (aio-handle-state s)]
                     [id (aio-next-id st)]
                     [r (aio-shutdown (aio-handle-handle s) id)])
                ($async-sync-slot-set! ss token (cons st id))
                (if (< r 0)
                    (begin
                      (deliver
                        (cons 'raise
                          (aio-io-condition 'shutdown s (aio-handle-path s) r)))
                      #f)
                    (begin
                      (aio-register-request! st id
                        (make-aio-req 'shutdown s deliver #f
                          (aio-plain-finish
                            (lambda (status aux)
                              (if (fx= status 0)
                                  (cons 'values '())
                                  (cons 'raise
                                    (aio-io-condition 'shutdown s (aio-handle-path s) status)))))
                          #f))
                      (list 'shutdown id))))))
        (lambda (vals) vals)
        (aio-request-nack token)))))

;;; The request identity belongs to one perform, not to the reusable operation.
(define aio-request-nack
  (lambda (token)
    (lambda (ss)
      (let ([attempt ($async-sync-slot-ref ss token #f)])
        (when attempt
          ($async-sync-slot-delete! ss token)
          (aio-cancel-request! (car attempt) (cdr attempt)))))))

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
                  (format "~a:~a" host port) #f (make-mutex)
                  #f #f '() #f #f '() '())])
         (define (fail r)
           (aio-handle-close h)
           (raise (aio-io-condition 'listen w (aio-handle-path w) r)))
         (let ([r (aio-tcp-bind h host port)])
           (when (fx< r 0) (fail r)))
         (let ([r (aio-listen-start h backlog)])
           (when (fx< r 0) (fail r)))
         (aio-register-handle! st w)
         (aio-bind-current-task-to-handle! w)
         w))]))

(define %tcp-accept-operation
  (lambda (listener)
    (unless (tcp-listener? listener)
      ($oops 'tcp-accept-operation "~s is not a tcp listener" listener))
    (let ([st (aio-handle-state listener)])
      (make-operation
        (lambda (ss)
          (aio-check-handle-owner! 'tcp-accept-operation listener)
          (with-mutex (aio-handle-mutex listener)
            (cond
              [(aio-handle-closing? listener)
               (cons 'raise (aio-closed-condition 'accept listener))]
              [(null? (aio-handle-accept-queue listener))
               (aio-attempt-accept st listener)]
              [else #f])))
        (lambda (ss deliver)
          (aio-check-handle-owner! 'tcp-accept-operation listener)
          (with-mutex (aio-handle-mutex listener)
            (cond
              [(aio-handle-closing? listener)
               (deliver (cons 'raise (aio-closed-condition 'accept listener)))
               #f]
              [(null? (aio-handle-accept-queue listener))
               (let ([payload (aio-attempt-accept st listener)])
                 (if payload
                     (begin (deliver payload) #f)
                     (begin
                       (aio-handle-accept-queue-set! listener
                         (list (cons ss deliver)))
                       (list 'accept (aio-handle-id listener)))))]
              [else
               (aio-handle-accept-queue-set! listener
                 (append (aio-handle-accept-queue listener)
                         (list (cons ss deliver))))
               (list 'accept (aio-handle-id listener))])))
        (lambda (vals)
          (for-each aio-bind-current-task-to-handle! vals)
          vals)
        (lambda (ss)
          (with-mutex (aio-handle-mutex listener)
            (aio-handle-accept-queue-set! listener
              (let loop ([q (aio-handle-accept-queue listener)])
                (cond
                  [(null? q) '()]
                  [(eq? (caar q) ss) (cdr q)]
                  [else (cons (car q) (loop (cdr q)))])))))))))

(define %tcp-connect-operation
  (lambda (host port)
    (aio-check-host-port 'tcp-connect-operation host port)
    (let ([token (list 'tcp-connect-operation)])
      (make-operation
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
                           (format "~a:~a" host port) #f (make-mutex)
                           #f #f '() #f #f '() '())])
                  (aio-register-handle! st w)
                  (aio-bind-current-task-to-handle! w)
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
       (let ([w (make-aio-handle id h 'pipe-listener st path #f (make-mutex)
                  #f #f '() #f #f '() '())])
         (define (fail r)
           (aio-handle-close h)
           (raise (aio-io-condition 'listen w path r)))
         (let ([r (aio-pipe-bind h path)])
           (when (fx< r 0) (fail r)))
         (let ([r (aio-listen-start h backlog)])
           (when (fx< r 0) (fail r)))
         (aio-register-handle! st w)
         (aio-bind-current-task-to-handle! w)
         w))]))

(define %pipe-connect-operation
  (lambda (path)
    (unless (string? path)
      ($oops 'pipe-connect-operation "~s is not a string" path))
    (let ([token (list 'pipe-connect-operation)])
      (make-operation
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
                (let ([w (make-aio-handle id h 'pipe-stream st path #f (make-mutex)
                           #f #f '() #f #f '() '())])
                  (aio-register-handle! st w)
                  (aio-bind-current-task-to-handle! w)
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
       (make-operation
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

;;; ---------------------------------------------------------------- files

;;; flag bits mirrored by aio_map_open_flags in c/asyncio.c
(define aio-open-flag-bits
  (lambda (flags)
    (fold-left
      (lambda (bits f)
        (case f
          [(read) (fxlogior bits 1)]
          [(write) (fxlogior bits 2)]
          [(create) (fxlogior bits 4)]
          [(truncate) (fxlogior bits 8)]
          [(append) (fxlogior bits 16)]
          [(exclusive) (fxlogior bits 32)]
          [else ($oops 'file-open "~s is not a file-open flag" f)]))
      0 flags)))

(define aio-fs-request-operation
  (case-lambda
    [(who handle path start gen)
     (aio-fs-request-operation who handle path start gen
       (lambda (ss status aux) (void)) #f)]
    [(who handle path start gen canceled-gen)
     (aio-fs-request-operation who handle path start gen canceled-gen #f)]
    [(who handle path start gen canceled-gen serial-file)
     ;; Callbacks receive ss so all mutable state belongs to this perform.
     (let ([token (list 'fs-request-operation)])
       (make-operation
         (lambda (ss) #f)
         (lambda (ss deliver)
           (when serial-file
             (aio-check-file-owner! who serial-file))
           (let ([st-box (box #f)] [id-box (box #f)]
                 [started? (box #f)] [entry-box (box #f)])
             ($async-sync-slot-set! ss token
               (vector st-box id-box started? entry-box))
             (letrec ([release!
                       (lambda ()
                         (when serial-file
                           (let ([next
                                  (with-mutex (async-file-mutex serial-file)
                                    (let ([q (async-file-queue serial-file)])
                                      (if (null? q)
                                          (begin
                                            (async-file-busy?-set! serial-file #f)
                                            #f)
                                          (let ([entry (car q)])
                                            (async-file-queue-set! serial-file (cdr q))
                                            (set-box! (cadr entry) #t)
                                            (caddr entry)))))])
                             (when next (next)))))]
                      [finish-normal
                       (lambda (status aux)
                         (dynamic-wind
                           (lambda () (void))
                           (lambda () (gen ss status aux))
                           release!))]
                      [finish-canceled
                       (lambda (status aux)
                         (dynamic-wind
                           (lambda () (void))
                           (lambda () (canceled-gen ss status aux))
                           release!))]
                      [submit
                     (lambda ()
                       (define submit-native
                         (lambda ()
                           (let ([st (aio-ensure-state! who)])
                             (when (and serial-file
                                        (not (eq? st
                                               (async-file-state serial-file))))
                               ($oops who
                                 "async file belongs to another scheduler"))
                             (let* ([id (aio-next-id st)]
                                    [r (start ss st id)])
                               (set-box! st-box st)
                               (set-box! id-box id)
                               (if (< r 0)
                                   (cons 'error r)
                                   (begin
                                     (aio-register-request! st id
                                       (make-aio-req 'fs handle deliver r
                                         (aio-fs-finish finish-normal
                                           finish-canceled)
                                         #f))
                                     (cons 'submitted id)))))))
                       (let ([result
                              (guard (c [else (cons 'exception c)])
                                (if serial-file
                                    (with-mutex (async-file-mutex serial-file)
                                      (if (aio-waiter-dead? ss)
                                          '(withdrawn)
                                          (submit-native)))
                                    (submit-native)))])
                         (case (car result)
                           [(submitted) (list 'fs (cdr result))]
                           [(withdrawn)
                            (release!)
                            #f]
                           [(error)
                            (release!)
                            (deliver
                              (cons 'raise
                                (aio-io-condition who handle path
                                  (cdr result))))
                            #f]
                           [else
                            (release!)
                            (deliver (cons 'raise (cdr result)))
                            #f])))])
             (if serial-file
                 (let ([entry (list ss started? submit)] [start-now? #f])
                   (set-box! entry-box entry)
                   (with-mutex (async-file-mutex serial-file)
                     (if (async-file-busy? serial-file)
                         (async-file-queue-set! serial-file
                           (append (async-file-queue serial-file)
                             (list entry)))
                         (begin
                           (async-file-busy?-set! serial-file #t)
                           (set-box! started? #t)
                           (set! start-now? #t))))
                   (when start-now? (submit))
                   (list 'file who))
                 (submit)))))
         (lambda (vals) vals)
         (lambda (ss)
           (let ([attempt ($async-sync-slot-ref ss token #f)])
             (when attempt
               (let ([st-box (vector-ref attempt 0)]
                     [id-box (vector-ref attempt 1)]
                     [started? (vector-ref attempt 2)]
                     [entry-box (vector-ref attempt 3)]
                     [nack-started? (not serial-file)])
                 (when serial-file
                   (with-mutex (async-file-mutex serial-file)
                     (if (unbox started?)
                         (set! nack-started? #t)
                         (async-file-queue-set! serial-file
                           (remq (unbox entry-box)
                             (async-file-queue serial-file))))))
                 (when nack-started?
                   (let ([st (unbox st-box)] [id (unbox id-box)])
                     (when (and st id) (aio-cancel-request! st id))))))))))]))

(define %file-open-operation
  (case-lambda
    [(path flags) (%file-open-operation path flags #o666)]
    [(path flags mode)
     (unless (string? path)
       ($oops 'file-open-operation "~s is not a string" path))
     (unless (and (list? flags) (for-all symbol? flags))
       ($oops 'file-open-operation "~s is not a list of flag symbols" flags))
     (unless (and (fixnum? mode) (fx>= mode 0) (fx<= mode #o7777))
       ($oops 'file-open-operation "~s is not a valid file mode" mode))
     (let ([bits (aio-open-flag-bits flags)]
           [token (list 'file-open-operation)])
       (define release-open-affinity!
         (lambda (ss)
           (let ([attempt ($async-sync-slot-ref ss token #f)])
             (when attempt
               (let* ([release-box (vector-ref attempt 2)]
                      [release (unbox release-box)])
                 (when release
                   (set-box! release-box #f)
                   (release)))))))
       (aio-fs-request-operation 'open #f path
         (lambda (ss st id)
           (let ([attempt
                  (vector st (current-async-task)
                    (box ($async-pin-current-task!)))])
             ($async-sync-slot-set! ss token attempt))
           (let ([r (aio-fs-open (aio-state-loop st) path bits mode id)])
             (when (< r 0) (release-open-affinity! ss))
             r))
         (lambda (ss status aux)
           (if (>= status 0)
               (let* ([attempt ($async-sync-slot-ref ss token #f)]
                      [st (vector-ref attempt 0)]
                      [owner (vector-ref attempt 1)]
                      [release-box (vector-ref attempt 2)]
                      [release (unbox release-box)])
                 (set-box! release-box #f)
                 (let ([f
                        (make-async-file% status path st #f
                          (if (fxlogtest bits 16) -1 0) ; append: track end lazily
                          #f (make-mutex) #f '()
                          (if release
                              (list (cons owner release))
                              '()))])
                   (cons 'values (list (aio-register-file! st f)))))
               (begin
                 (release-open-affinity! ss)
                 (cons 'raise (aio-io-condition 'open #f path status)))))
         (lambda (ss status aux)
           (when (>= status 0)
             (aio-fs-close-now status))
           (release-open-affinity! ss))))]))

(define aio-check-file
  (lambda (who f)
    (unless (%async-file? f)
      ($oops who "~s is not an async file" f))
    (when (async-file-closed? f)
      (raise (aio-io-condition who f (async-file-path f) 'closed)))))

(define aio-check-file-unowned
  (lambda (who f)
    (aio-check-file who f)
    (when (async-file-port-owned? f)
      ($oops who "async file ownership has been transferred to a port"))))

(define aio-check-file-owner!
  (lambda (who f)
    (let ([sched (current-async-scheduler)])
      (unless (and sched
                   (eq? ($async-scheduler-io-state sched)
                        (async-file-state f)))
        ($oops who "async file belongs to another scheduler"))
      (aio-bind-current-task-to-file! f))))

(define aio-claim-file-for-port!
  (lambda (who f)
    (aio-check-file who f)
    (aio-check-file-owner! who f)
    (with-mutex (async-file-mutex f)
      (when (async-file-port-owned? f)
        ($oops who "async file ownership has already been transferred to a port"))
      (async-file-port-owned?-set! f #t))))

(define aio-file-offset!
  (lambda (f n)
    ;; an offset of -1 means the append/current position
    (let ([off (async-file-offset f)])
      (when (and (>= n 0) (>= off 0))
        (async-file-offset-set! f (+ off n)))
      off)))

(define %file-read-operation
  (lambda (f len . allow-owned-option)
    (aio-check-file 'file-read-operation f)
    (unless (and (fixnum? len) (fx> len 0) (fx<= len (expt 2 30)))
      ($oops 'file-read-operation "~s is not a positive fixnum length" len))
    (let ([allow-owned? (and (pair? allow-owned-option)
                             (car allow-owned-option))])
      (aio-fs-request-operation 'read f (async-file-path f)
      (lambda (ss st id)
        (if allow-owned?
            (aio-check-file 'file-read-operation f)
            (aio-check-file-unowned 'file-read-operation f))
        (aio-fs-read (aio-state-loop st) (async-file-fd f) len
          (aio-file-offset! f 0) id))
      (lambda (ss status aux)
        (cond
          [(fx> status 0)
           (aio-file-offset! f status)
           (let ([bv (make-bytevector status)])
             (aio-read-copy (aio-fs-data aux) bv status)
             (cons 'values (list bv)))]
          [(fx= status 0) (cons 'values (list #!eof))]
          [else
           (cons 'raise
             (aio-io-condition 'read f (async-file-path f) status))]))
      (lambda (ss status aux)
        (when (fx>= status 0) (aio-file-offset! f status)))
      f))))

(define %file-write-operation
  (lambda (f bv . allow-owned-option)
    (aio-check-file 'file-write-operation f)
    (unless (bytevector? bv)
      ($oops 'file-write-operation "~s is not a bytevector" bv))
    (let ([allow-owned? (and (pair? allow-owned-option)
                             (car allow-owned-option))])
      (aio-fs-request-operation 'write f (async-file-path f)
      (lambda (ss st id)
        (if allow-owned?
            (aio-check-file 'file-write-operation f)
            (aio-check-file-unowned 'file-write-operation f))
        (aio-fs-write (aio-state-loop st) (async-file-fd f) bv
          (bytevector-length bv) (aio-file-offset! f 0) id))
      (lambda (ss status aux)
        (if (>= status 0)
            (begin
              (aio-file-offset! f status)
              (cons 'values (list status)))
            (cons 'raise
              (aio-io-condition 'write f (async-file-path f) status))))
      (lambda (ss status aux)
        (when (fx>= status 0) (aio-file-offset! f status)))
      f))))

(define %file-close-operation
  (lambda (f)
    (aio-fs-request-operation 'close f (async-file-path f)
      (lambda (ss st id)
        (aio-fs-close-fd (aio-state-loop st) (async-file-fd f) id))
      (lambda (ss status aux)
        (if (fx= status 0)
            (begin
              (async-file-closed?-set! f #t)
              (aio-unregister-file! f)
              (aio-release-file-affinities! f)
              (cons 'values '()))
            (cons 'raise
              (aio-io-condition 'close f (async-file-path f) status))))
      (lambda (ss status aux)
        (when (fx= status 0)
          (async-file-closed?-set! f #t)
          (aio-unregister-file! f)
          (aio-release-file-affinities! f)))
      f)))

(define aio-stat-alist
  (lambda (aux)
    (define (field i) (aio-fs-stat-field aux i))
    (list
      (cons 'dev (field 0))
      (cons 'mode (field 1))
      (cons 'nlink (field 2))
      (cons 'uid (field 3))
      (cons 'gid (field 4))
      (cons 'rdev (field 5))
      (cons 'ino (field 6))
      (cons 'size (field 7))
      (cons 'blksize (field 8))
      (cons 'blocks (field 9))
      (cons 'atime (cons (field 12) (field 13)))
      (cons 'mtime (cons (field 14) (field 15)))
      (cons 'ctime (cons (field 16) (field 17))))))

;;; ------------------------------------------------------- port adapters

(define aio-port-read-procedure
  (lambda (read-chunk)
    (let ([pending #f] [pending-start 0])
      (lambda (target start count)
        (if (fx= count 0)
            0
            (let loop ()
              (if pending
                  (let* ([available
                          (fx- (bytevector-length pending) pending-start)]
                         [n (fxmin available count)])
                    (bytevector-copy! pending pending-start target start n)
                    (if (fx= n available)
                        (begin
                          (set! pending #f)
                          (set! pending-start 0))
                        (set! pending-start (fx+ pending-start n)))
                    n)
                  (let ([chunk (read-chunk count)])
                    (cond
                      [(eof-object? chunk) 0]
                      [(fx= (bytevector-length chunk) 0) (loop)]
                      [else
                       (set! pending chunk)
                       (set! pending-start 0)
                       (loop)])))))))))

(define aio-port-write-procedure
  (lambda (write-chunk)
    (lambda (source start count)
      (if (fx= count 0)
          0
          (let* ([chunk
                  (if (and (fx= start 0)
                           (fx= count (bytevector-length source)))
                      source
                      (let ([copy (make-bytevector count)])
                        (bytevector-copy! source start copy 0 count)
                        copy))]
                 [n (write-chunk chunk)])
            (when (fx= n 0)
              ($oops 'async-handle-port "write made no progress"))
            n)))))

(define aio-stream-port-read-procedure
  (lambda (s)
    (aio-port-read-procedure
      (lambda (count)
        (perform-operation (%stream-read-operation s #t))))))

(define aio-stream-port-write-procedure
  (lambda (s)
    (aio-port-write-procedure
      (lambda (bv)
        (let ([n (bytevector-length bv)])
          (perform-operation (%stream-write-operation s bv #t))
          n)))))

(define aio-file-port-read-procedure
  (lambda (f)
    (aio-port-read-procedure
      (lambda (count)
        (perform-operation (%file-read-operation f count #t))))))

(define aio-file-port-write-procedure
  (lambda (f)
    (aio-port-write-procedure
      (lambda (bv)
        (perform-operation (%file-write-operation f bv #t))))))

(define aio-file-port-position-procedures
  (lambda (f)
    (if (fx>= (async-file-offset f) 0)
        (values
          (lambda ()
            (aio-check-file 'port-position f)
            (async-file-offset f))
          (lambda (position)
            (aio-check-file 'set-port-position! f)
            (async-file-offset-set! f position)))
        (values #f #f))))

(define aio-close-file-from-port
  (lambda (f)
    (aio-check-file 'close-port f)
    (perform-operation (%file-close-operation f))))

(define aio-file-port-id
  (lambda (f)
    (format "async file ~a" (async-file-path f))))

(define aio-stream-port-id
  (lambda (s)
    (let ([path (aio-handle-path s)])
      (if path
          (format "async stream ~a" path)
          "async stream"))))

(define %file-stat-operation
  (lambda (target)
    (cond
      [(string? target)
       (aio-fs-request-operation 'stat #f target
         (lambda (ss st id) (aio-fs-stat (aio-state-loop st) target id))
         (lambda (ss status aux)
           (if (fx= status 0)
               (cons 'values (list (aio-stat-alist aux)))
               (cons 'raise (aio-io-condition 'stat #f target status)))))]
      [(%async-file? target)
       (aio-check-file 'file-stat-operation target)
       (aio-fs-request-operation 'stat target (async-file-path target)
         (lambda (ss st id)
           (aio-check-file-owner! 'file-stat-operation target)
           (aio-check-file-unowned 'file-stat-operation target)
           (aio-fs-fstat (aio-state-loop st) (async-file-fd target) id))
         (lambda (ss status aux)
           (if (fx= status 0)
               (cons 'values (list (aio-stat-alist aux)))
               (cons 'raise
                 (aio-io-condition 'stat target (async-file-path target) status)))))]
      [else
       ($oops 'file-stat-operation "~s is not a path or async file" target)])))

;;; ------------------------------------------------------- public exports

(set! make-async-io-condition
  (lambda (operation handle path code)
    (make-async-io-condition% operation handle path code)))

(set! async-io-condition? %async-io-condition?)
(set! async-io-condition-operation %async-io-condition-operation)
(set! async-io-condition-handle %async-io-condition-handle)
(set! async-io-condition-path %async-io-condition-path)
(set! async-io-condition-code %async-io-condition-code)

(set-who! async-io-error-name
  (lambda (code)
    (unless (fixnum? code) ($oops who "~s is not a fixnum error code" code))
    (aio-resolve-kernel!)
    (let ([buf (make-bytevector 64)])
      (aio-err-name-into code buf 64)
      (bv->cstring buf))))

(set-who! async-io-error-message
  (lambda (code)
    (unless (fixnum? code) ($oops who "~s is not a fixnum error code" code))
    (aio-resolve-kernel!)
    (let ([buf (make-bytevector 256)])
      (aio-strerror-into code buf 256)
      (bv->cstring buf))))

(set! tcp-listener?
  (lambda (x)
    (and (aio-handle? x)
         (or (eq? (aio-handle-kind x) 'tcp-listener)
             (eq? (aio-handle-kind x) 'pipe-listener)))))
(set-who! tcp-listener-close
  (lambda (listener)
    (unless (tcp-listener? listener)
      ($oops who "~s is not a listener" listener))
    (aio-close-owned-handle who listener)))
(set-who! tcp-accept
  (lambda (listener)
    (perform-operation (%tcp-accept-operation listener))))
(set-who! tcp-connect
  (lambda (host port)
    (perform-operation (%tcp-connect-operation host port))))
(set-who! pipe-connect
  (lambda (path)
    (perform-operation (%pipe-connect-operation path))))

(set! async-stream?
  (lambda (x)
    (and (aio-handle? x)
         (or (eq? (aio-handle-kind x) 'tcp-stream)
             (eq? (aio-handle-kind x) 'pipe-stream)))))
(set! tcp-stream?
  (lambda (x)
    (and (aio-handle? x) (eq? (aio-handle-kind x) 'tcp-stream))))
(set! pipe-stream?
  (lambda (x)
    (and (aio-handle? x) (eq? (aio-handle-kind x) 'pipe-stream))))
(set-who! stream-close
  (lambda (s)
    (aio-check-stream-unowned who s)
    (aio-close-owned-handle who s)))
(set! stream-closed?
  (lambda (s)
    (and (aio-handle? s)
         (or (aio-handle-closing? s) (aio-handle-closed? s)))))
(set-who! stream-read
  (lambda (s)
    (aio-check-stream-unowned who s)
    (perform-operation (%stream-read-operation s))))
(set-who! stream-write
  (lambda (s bv)
    (aio-check-stream-unowned who s)
    (perform-operation (%stream-write-operation s bv))
    (void)))
(set-who! stream-shutdown
  (lambda (s)
    (aio-check-stream-unowned who s)
    (perform-operation (stream-shutdown-operation s))
    (void)))
(set-who! async-stream->binary-input-port
  (lambda (s)
    (aio-claim-stream-for-port! who s)
    (make-custom-binary-input-port (aio-stream-port-id s)
      (aio-stream-port-read-procedure s)
      #f #f
      (lambda () (aio-close-owned-handle who s)))))
(set-who! async-stream->binary-output-port
  (lambda (s)
    (aio-claim-stream-for-port! who s)
    (make-custom-binary-output-port (aio-stream-port-id s)
      (aio-stream-port-write-procedure s)
      #f #f
      (lambda () (aio-close-owned-handle who s)))))
(set-who! async-stream->binary-input/output-port
  (lambda (s)
    (aio-claim-stream-for-port! who s)
    (let ([p
           (make-custom-binary-input/output-port (aio-stream-port-id s)
             (aio-stream-port-read-procedure s)
             (aio-stream-port-write-procedure s)
             #f #f
             (lambda () (aio-close-owned-handle who s)))])
      ;; A stream cannot seek backward over input prefetched by a duplex port.
      ($reset-port-flags! p (constant port-flag-block-buffered))
      p)))

(set! dns-lookup
  (case-lambda
    [(node) (perform-operation (%dns-lookup-operation node))]
    [(node service) (perform-operation (%dns-lookup-operation node service))]))

(set! async-file? %async-file?)
(set! file-open
  (case-lambda
    [(path flags) (perform-operation (%file-open-operation path flags))]
    [(path flags mode) (perform-operation (%file-open-operation path flags mode))]))
(set-who! file-read
  (lambda (f len)
    (aio-check-file-unowned who f)
    (perform-operation (%file-read-operation f len))))
(set-who! file-write
  (lambda (f bv)
    (aio-check-file-unowned who f)
    (perform-operation (%file-write-operation f bv))))
(set-who! file-close
  (lambda (f)
    (aio-check-file-unowned who f)
    (perform-operation (%file-close-operation f))
    (void)))
(set-who! async-file->binary-input-port
  (lambda (f)
    (aio-claim-file-for-port! who f)
    (let-values ([(get-position set-position!)
                  (aio-file-port-position-procedures f)])
      (make-custom-binary-input-port (aio-file-port-id f)
        (aio-file-port-read-procedure f)
        get-position set-position!
        (lambda () (aio-close-file-from-port f))))))
(set-who! async-file->binary-output-port
  (lambda (f)
    (aio-claim-file-for-port! who f)
    (let-values ([(get-position set-position!)
                  (aio-file-port-position-procedures f)])
      (make-custom-binary-output-port (aio-file-port-id f)
        (aio-file-port-write-procedure f)
        get-position set-position!
        (lambda () (aio-close-file-from-port f))))))
(set-who! async-file->binary-input/output-port
  (lambda (f)
    (aio-claim-file-for-port! who f)
    (let-values ([(get-position set-position!)
                  (aio-file-port-position-procedures f)])
      (make-custom-binary-input/output-port (aio-file-port-id f)
        (aio-file-port-read-procedure f)
        (aio-file-port-write-procedure f)
        get-position set-position!
        (lambda () (aio-close-file-from-port f))))))
(set-who! file-stat
  (lambda (target)
    (when (%async-file? target)
      (aio-check-file-unowned who target))
    (perform-operation (%file-stat-operation target))))
(set-who! file-delete
  (lambda (path)
    (unless (string? path) ($oops who "~s is not a string" path))
    (perform-operation
      (aio-fs-request-operation 'delete #f path
        (lambda (ss st id) (aio-fs-unlink (aio-state-loop st) path id))
        (lambda (ss status aux)
          (if (fx= status 0)
              (cons 'values '())
              (cons 'raise (aio-io-condition 'delete #f path status))))))
    (void)))
(set-who! file-rename
  (lambda (old new)
    (unless (string? old) ($oops who "~s is not a string" old))
    (unless (string? new) ($oops who "~s is not a string" new))
    (perform-operation
      (aio-fs-request-operation 'rename #f old
        (lambda (ss st id) (aio-fs-rename (aio-state-loop st) old new id))
        (lambda (ss status aux)
          (if (fx= status 0)
              (cons 'values '())
              (cons 'raise (aio-io-condition 'rename #f old status))))))
    (void)))
(set-who! directory-create
  (case-lambda
    [(path) (directory-create path #o755)]
    [(path mode)
     (unless (string? path) ($oops who "~s is not a string" path))
     (perform-operation
       (aio-fs-request-operation 'mkdir #f path
         (lambda (ss st id) (aio-fs-mkdir (aio-state-loop st) path mode id))
         (lambda (ss status aux)
           (if (fx= status 0)
               (cons 'values '())
               (cons 'raise (aio-io-condition 'mkdir #f path status))))))
     (void)]))

(set! tcp-listen %tcp-listen)
(set! tcp-accept-operation %tcp-accept-operation)
(set! tcp-connect-operation %tcp-connect-operation)
(set! pipe-listen %pipe-listen)
(set! pipe-connect-operation %pipe-connect-operation)
(set-who! stream-read-operation
  (lambda (s)
    (aio-check-stream-unowned who s)
    (%stream-read-operation s)))
(set-who! stream-write-operation
  (lambda (s bv)
    (aio-check-stream-unowned who s)
    (%stream-write-operation s bv)))
(set! dns-lookup-operation %dns-lookup-operation)
(set! file-open-operation %file-open-operation)
(set-who! file-read-operation
  (lambda (f len)
    (aio-check-file-unowned who f)
    (%file-read-operation f len)))
(set-who! file-write-operation
  (lambda (f bv)
    (aio-check-file-unowned who f)
    (%file-write-operation f bv)))
(set-who! file-stat-operation
  (lambda (target)
    (when (%async-file? target)
      (aio-check-file-unowned who target))
    (%file-stat-operation target)))

;;; the scheduler calls this when run-async exits
(set! $async-io-shutdown aio-io-shutdown)

(record-writer (type-descriptor aio-handle)
  (lambda (r p wr)
    (fprintf p "#<async-~a ~a~a>"
      (aio-handle-kind r)
      (or (aio-handle-path r) (aio-handle-id r))
      (if (aio-handle-closed? r) " closed" ""))))

(record-writer (type-descriptor async-file)
  (lambda (r p wr)
    (fprintf p "#<async-file ~a~a>"
      (async-file-path r)
      (if (async-file-closed? r) " closed" ""))))
)
