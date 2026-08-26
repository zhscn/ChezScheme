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
             (eq? (aio-handle-kind x) 'pipe-stream)
             (eq? (aio-handle-kind x) 'tty-stream)))))
(set! tcp-stream?
  (lambda (x)
    (and (aio-handle? x) (eq? (aio-handle-kind x) 'tcp-stream))))
(set! pipe-stream?
  (lambda (x)
    (and (aio-handle? x) (eq? (aio-handle-kind x) 'pipe-stream))))
(set! tty-stream?
  (lambda (x)
    (and (aio-handle? x) (eq? (aio-handle-kind x) 'tty-stream))))
(set-who! stream-close
  (lambda (s)
    (aio-check-stream-unowned who s)
    (aio-close-owned-handle who s)))
(set! stream-closed?
  (lambda (s)
    (and (aio-handle? s)
         (with-mutex (aio-handle-mutex s)
           (or (aio-handle-closing? s) (aio-handle-closed? s))))))
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
(set! dns-reverse-operation %dns-reverse-operation)
(set! dns-reverse
  (case-lambda
    [(host port) (perform-operation (%dns-reverse-operation host port))]
    [(host port flags)
     (perform-operation (%dns-reverse-operation host port flags))]))
(set! random-bytevector-operation %random-bytevector-operation)
(set-who! random-bytevector
  (lambda (length)
    (perform-operation (%random-bytevector-operation length))))
(set! fd-poll-open %fd-poll-open)
(set! fd-poll-handle?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'poll))))
(set-who! fd-poll-close
  (lambda (poll)
    (unless (fd-poll-handle? poll)
      ($oops who "~s is not an fd poll handle" poll))
    (aio-close-owned-handle who poll)))
(set! fd-poll-operation %fd-poll-operation)
(set-who! fd-poll
  (lambda (poll events)
    (perform-operation (%fd-poll-operation poll events))))
(set! process-spawn %process-spawn)
(set! async-process?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'process))))
(set-who! process-id
  (lambda (process)
    (unless (async-process? process)
      ($oops who "~s is not an async process" process))
    (aio-process-pid (aio-handle-handle process))))
(set! process-wait-operation %process-wait-operation)
(set-who! process-wait
  (lambda (process)
    (perform-operation (%process-wait-operation process))))
(set-who! process-kill
  (lambda (process signal)
    (unless (async-process? process)
      ($oops who "~s is not an async process" process))
    (unless (fixnum? signal) ($oops who "~s is not a signal number" signal))
    (aio-check-handle-scope! who process)
    (let ([r (aio-process-kill (aio-handle-handle process) signal)])
      (when (fx< r 0)
        (raise (aio-io-condition 'process-kill process
                 (aio-handle-path process) r))))
    (void)))
(set-who! process-id-kill
  (lambda (pid signal)
    (unless (and (integer? pid) (exact? pid))
      ($oops who "~s is not a process id" pid))
    (unless (fixnum? signal) ($oops who "~s is not a signal number" signal))
    (let ([r (aio-kill pid signal)])
      (when (fx< r 0)
        (raise (aio-io-condition 'process-kill #f #f r))))
    (void)))
(set-who! process-close
  (lambda (process)
    (unless (async-process? process)
      ($oops who "~s is not an async process" process))
    (aio-close-owned-handle who process)))
(set! signal-open %signal-open)
(set! signal-watcher?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'signal))))
(set! signal-receive-operation %signal-receive-operation)
(set-who! signal-receive
  (lambda (watcher)
    (perform-operation (%signal-receive-operation watcher))))
(set-who! signal-close
  (lambda (watcher)
    (aio-check-watcher who watcher 'signal)
    (aio-close-owned-handle who watcher)))
(set! fs-event-open %fs-event-open)
(set! fs-event-watcher?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'fs-event))))
(set! fs-event-receive-operation %fs-event-receive-operation)
(set-who! fs-event-receive
  (lambda (watcher)
    (perform-operation (%fs-event-receive-operation watcher))))
(set-who! fs-event-close
  (lambda (watcher)
    (aio-check-watcher who watcher 'fs-event)
    (aio-close-owned-handle who watcher)))
(set! fs-poll-open %fs-poll-open)
(set! fs-poll-watcher?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'fs-poll))))
(set! fs-poll-receive-operation %fs-poll-receive-operation)
(set-who! fs-poll-receive
  (lambda (watcher)
    (perform-operation (%fs-poll-receive-operation watcher))))
(set-who! fs-poll-close
  (lambda (watcher)
    (aio-check-watcher who watcher 'fs-poll)
    (aio-close-owned-handle who watcher)))
(set! tty-open %tty-open)
(set-who! tty-mode-set!
  (lambda (tty mode)
    (unless (tty-stream? tty) ($oops who "~s is not a TTY stream" tty))
    (let ([value (case mode [(normal) 0] [(raw) 1] [(io) 2]
                   [else ($oops who "~s is not a TTY mode" mode)])])
      (aio-check-handle-scope! who tty)
      (let ([r (aio-tty-set-mode (aio-handle-handle tty) value)])
        (when (fx< r 0)
          (raise (aio-io-condition who tty (aio-handle-path tty) r))))
      (void))))
(set-who! tty-window-size
  (lambda (tty)
    (unless (tty-stream? tty) ($oops who "~s is not a TTY stream" tty))
    (aio-check-handle-scope! who tty)
    (let ([width (aio-tty-winsize (aio-handle-handle tty) 0)]
          [height (aio-tty-winsize (aio-handle-handle tty) 1)])
      (when (fx< width 0)
        (raise (aio-io-condition who tty (aio-handle-path tty) width)))
      (when (fx< height 0)
        (raise (aio-io-condition who tty (aio-handle-path tty) height)))
      (values width height))))
(set-who! tty-virtual-terminal-state
  (lambda ()
    (aio-resolve-kernel!)
    (let ([r (aio-tty-get-vterm-state)])
      (if (fx< r 0)
          (raise (aio-io-condition who #f #f r))
          (if (fx= r 0) 'unsupported 'supported)))))
(set-who! tty-virtual-terminal-state-set!
  (lambda (state)
    (unless (memq state '(supported unsupported))
      ($oops who "~s is not a virtual terminal state" state))
    (aio-resolve-kernel!)
    (aio-tty-set-vterm-state (if (eq? state 'supported) 1 0))
    (void)))
(set-who! tty-reset-mode!
  (lambda ()
    (aio-resolve-kernel!)
    (aio-tty-reset-mode)
    (void)))
(set-who! system-high-resolution-time
  (lambda () (aio-resolve-kernel!) (aio-system-u64 0)))
(set-who! system-memory-info
  (lambda ()
    (aio-resolve-kernel!)
    (list (cons 'total (aio-system-u64 1))
          (cons 'free (aio-system-u64 2))
          (cons 'constrained (aio-system-u64 3))
          (cons 'available (aio-system-u64 4))
          (cons 'resident-set (aio-system-u64 5)))))
(set-who! system-uptime
  (lambda () (aio-resolve-kernel!) (aio-system-double 0)))
(set-who! system-load-average
  (lambda ()
    (aio-resolve-kernel!)
    (list (aio-system-double 1) (aio-system-double 2)
          (aio-system-double 3))))
(set-who! system-process-info
  (lambda ()
    (aio-resolve-kernel!)
    (list (cons 'pid (aio-system-u64 6))
          (cons 'parent-pid (aio-system-u64 7))
          (cons 'available-parallelism (aio-system-u64 8)))))
(set-who! system-path-info
  (lambda ()
    (list (cons 'executable (aio-system-cstring who aio-system-string 0))
          (cons 'current-directory
            (aio-system-cstring who aio-system-string 1))
          (cons 'home-directory
            (aio-system-cstring who aio-system-string 2))
          (cons 'temporary-directory
            (aio-system-cstring who aio-system-string 3))
          (cons 'hostname (aio-system-cstring who aio-system-string 4)))))
(set-who! system-uname
  (lambda ()
    (list (cons 'system (aio-system-cstring who aio-uname-string 0))
          (cons 'release (aio-system-cstring who aio-uname-string 1))
          (cons 'version (aio-system-cstring who aio-uname-string 2))
          (cons 'machine (aio-system-cstring who aio-uname-string 3)))))
(set-who! system-cpu-info (lambda () (%system-cpu-info)))
(set-who! system-interface-info (lambda () (%system-interface-info)))
(set-who! system-resource-usage (lambda () (%system-resource-usage)))
(set-who! async-loop-metrics (lambda () (%async-loop-metrics)))

(set! udp-open %udp-open)
(set! udp-socket?
  (lambda (x) (and (aio-handle? x) (eq? (aio-handle-kind x) 'udp))))
(set-who! udp-close
  (lambda (socket)
    (aio-check-udp who socket)
    (aio-close-owned-handle who socket)))
(set! udp-send-operation %udp-send-operation)
(set! udp-send
  (case-lambda
    [(socket bv) (perform-operation (%udp-send-operation socket bv))]
    [(socket bv host port)
     (perform-operation (%udp-send-operation socket bv host port))]))
(set! udp-receive-operation %udp-receive-operation)
(set-who! udp-receive
  (lambda (socket) (perform-operation (%udp-receive-operation socket))))
(set! udp-bind!
  (case-lambda
    [(socket host port) (udp-bind! socket host port '())]
    [(socket host port flags)
     (aio-check-host-port 'udp-bind! host port)
     (aio-udp-control 'udp-bind socket
       (lambda ()
         (aio-udp-bind (aio-handle-handle socket) host port
           (aio-udp-bind-flag-bits 'udp-bind! flags))))
     (void)]))
(set-who! udp-connect!
  (lambda (socket host port)
    (aio-check-host-port who host port)
    (aio-udp-control 'udp-connect socket
      (lambda ()
        (aio-udp-connect (aio-handle-handle socket) host port)))
    (void)))
(set-who! udp-disconnect!
  (lambda (socket)
    (aio-udp-control 'udp-disconnect socket
      (lambda () (aio-udp-connect (aio-handle-handle socket) "" 0)))
    (void)))
(set-who! udp-local-address
  (lambda (socket) (apply values (aio-udp-address-list who socket #f))))
(set-who! udp-peer-address
  (lambda (socket) (apply values (aio-udp-address-list who socket #t))))
(set-who! udp-membership-set!
  (lambda (socket multicast interface source action)
    (unless (string? multicast) ($oops who "~s is not a string" multicast))
    (unless (string? interface) ($oops who "~s is not a string" interface))
    (unless (string? source) ($oops who "~s is not a string" source))
    (unless (memq action '(join leave))
      ($oops who "~s is not a membership action" action))
    (aio-udp-control who socket
      (lambda ()
        (aio-udp-set-membership (aio-handle-handle socket)
          multicast interface source (if (eq? action 'join) 1 0))))
    (void)))
(set-who! udp-multicast-interface-set!
  (lambda (socket interface)
    (unless (string? interface) ($oops who "~s is not a string" interface))
    (aio-udp-control who socket
      (lambda ()
        (aio-udp-set-multicast-interface (aio-handle-handle socket)
          interface)))
    (void)))
(set-who! udp-option-set!
  (lambda (socket option value)
    (let ([index
           (case option
             [(multicast-loop) 0]
             [(multicast-ttl) 1]
             [(broadcast) 3]
             [(ttl) 4]
             [else ($oops who "~s is not a UDP option" option)])])
      (unless (and (integer? value) (exact? value))
        ($oops who "~s is not an exact integer" value))
      (aio-udp-control who socket
        (lambda ()
          (aio-udp-set-option (aio-handle-handle socket) index value)))
      (void))))

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
    (perform-operation (%file-delete-operation path))
    (void)))
(set-who! file-rename
  (lambda (old new)
    (perform-operation (%file-rename-operation old new))
    (void)))
(set-who! directory-create
  (case-lambda
    [(path) (directory-create path #o755)]
    [(path mode)
     (perform-operation (%directory-create-operation path mode))
     (void)]))
(set-who! directory-delete
  (lambda (path)
    (perform-operation (%directory-delete-operation path))
    (void)))
(set! file-copy
  (case-lambda
    [(from to) (perform-operation (%file-copy-operation from to))]
    [(from to flags)
     (perform-operation (%file-copy-operation from to flags))]
    ))
(set-who! temporary-directory-create
  (lambda (pattern)
    (perform-operation (%temporary-directory-create-operation pattern))))
(set-who! temporary-file-open
  (lambda (pattern)
    (perform-operation (%temporary-file-open-operation pattern))))
(set-who! directory-scan
  (lambda (path) (perform-operation (%directory-scan-operation path))))
(set! async-directory? %async-directory?)
(set-who! directory-open
  (lambda (path) (perform-operation (%directory-open-operation path))))
(set! directory-read
  (case-lambda
    [(directory) (perform-operation (%directory-read-operation directory))]
    [(directory count)
     (perform-operation (%directory-read-operation directory count))]))
(set-who! directory-close
  (lambda (directory)
    (perform-operation (%directory-close-operation directory))
    (void)))
(set-who! file-sync
  (lambda (f)
    (perform-operation (%file-sync-operation f #f))
    (void)))
(set-who! file-data-sync
  (lambda (f)
    (perform-operation (%file-sync-operation f #t))
    (void)))
(set-who! file-truncate
  (lambda (f length)
    (perform-operation (%file-truncate-operation f length))
    (void)))
(set! file-send-operation %file-send-operation)
(set-who! file-send
  (lambda (output input offset length)
    (perform-operation (%file-send-operation output input offset length))))
(set-who! file-access?
  (lambda (path modes)
    (perform-operation (%file-access-operation path modes))))
(set-who! file-mode-set!
  (lambda (target mode)
    (perform-operation (%file-mode-set-operation target mode))
    (void)))
(set! file-times-set!
  (case-lambda
    [(target atime mtime)
     (perform-operation (%file-times-set-operation target atime mtime))
     (void)]
    [(target atime mtime follow?)
     (perform-operation
       (%file-times-set-operation target atime mtime follow?))
     (void)]))
(set-who! file-lstat
  (lambda (path) (perform-operation (%file-lstat-operation path))))
(set-who! file-link
  (lambda (from to)
    (perform-operation (%file-link-operation from to #f '()))
    (void)))
(set! file-symbolic-link
  (case-lambda
    [(from to) (file-symbolic-link from to '())]
    [(from to flags)
     (perform-operation (%file-link-operation from to #t flags))
     (void)]))
(set-who! file-read-link
  (lambda (path)
    (perform-operation (%file-read-link-operation path #f))))
(set-who! file-real-path
  (lambda (path)
    (perform-operation (%file-read-link-operation path #t))))
(set! file-owner-set!
  (case-lambda
    [(target uid gid)
     (perform-operation (%file-owner-set-operation target uid gid))
     (void)]
    [(target uid gid follow?)
     (perform-operation (%file-owner-set-operation target uid gid follow?))
     (void)]))
(set-who! file-system-stat
  (lambda (path)
    (perform-operation (%file-system-stat-operation path))))

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
(set! file-close-operation %file-close-operation)
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
(set! file-delete-operation %file-delete-operation)
(set! file-rename-operation %file-rename-operation)
(set! directory-create-operation %directory-create-operation)
(set! directory-delete-operation %directory-delete-operation)
(set! file-copy-operation %file-copy-operation)
(set! temporary-directory-create-operation
  %temporary-directory-create-operation)
(set! temporary-file-open-operation %temporary-file-open-operation)
(set! directory-scan-operation %directory-scan-operation)
(set! directory-open-operation %directory-open-operation)
(set! directory-read-operation %directory-read-operation)
(set! directory-close-operation %directory-close-operation)
(set-who! file-sync-operation
  (lambda (f) (%file-sync-operation f #f)))
(set-who! file-data-sync-operation
  (lambda (f) (%file-sync-operation f #t)))
(set! file-truncate-operation %file-truncate-operation)
(set! file-access-operation %file-access-operation)
(set! file-mode-set-operation %file-mode-set-operation)
(set! file-times-set-operation %file-times-set-operation)
(set! file-lstat-operation %file-lstat-operation)
(set-who! file-link-operation
  (lambda (from to) (%file-link-operation from to #f '())))
(set! file-symbolic-link-operation
  (case-lambda
    [(from to) (%file-link-operation from to #t '())]
    [(from to flags) (%file-link-operation from to #t flags)]))
(set-who! file-read-link-operation
  (lambda (path) (%file-read-link-operation path #f)))
(set-who! file-real-path-operation
  (lambda (path) (%file-read-link-operation path #t)))
(set! file-owner-set-operation %file-owner-set-operation)
(set! file-system-stat-operation %file-system-stat-operation)

;;; the scheduler calls this when run-async exits
(set! $async-io-shutdown aio-io-shutdown)

(record-writer (type-descriptor aio-handle)
  (lambda (r p wr)
    (let ([closed?
           (with-mutex (aio-handle-mutex r)
             (aio-handle-closed? r))])
      (fprintf p "#<async-~a ~a~a>"
        (aio-handle-kind r)
        (or (aio-handle-path r) (aio-handle-id r))
        (if closed? " closed" "")))))

(record-writer (type-descriptor async-file)
  (lambda (r p wr)
    (let ([closed?
           (with-mutex (async-file-mutex r)
             (async-file-closed? r))])
      (fprintf p "#<async-file ~a~a>"
        (async-file-path r)
        (if closed? " closed" "")))))
