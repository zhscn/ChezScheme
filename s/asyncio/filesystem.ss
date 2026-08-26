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

(define aio-serial-state
  (lambda (resource)
    (if (%async-file? resource)
        (async-file-state resource)
        (async-directory-state resource))))

(define aio-serial-mutex
  (lambda (resource)
    (if (%async-file? resource)
        (async-file-mutex resource)
        (async-directory-mutex resource))))

(define aio-serial-busy?
  (lambda (resource)
    (if (%async-file? resource)
        (async-file-busy? resource)
        (async-directory-busy? resource))))

(define aio-serial-busy?-set!
  (lambda (resource value)
    (if (%async-file? resource)
        (async-file-busy?-set! resource value)
        (async-directory-busy?-set! resource value))))

(define aio-serial-queue
  (lambda (resource)
    (if (%async-file? resource)
        (async-file-queue resource)
        (async-directory-queue resource))))

(define aio-check-serial-scope!
  (lambda (who resource)
    (if (%async-file? resource)
        (aio-check-file-scope! who resource)
        (let ([sched (current-async-scheduler)])
          (unless (and sched
                       (eq? ($async-scheduler-group-token sched)
                            ($async-scheduler-group-token
                              (aio-state-owner
                                (async-directory-state resource)))))
            ($oops who "async directory belongs to another scheduler group"))))))

(define aio-fs-request-operation
  (case-lambda
    [(who handle path start gen)
     (aio-fs-request-operation who handle path start gen
       (lambda (ss status aux) (void)) #f)]
    [(who handle path start gen canceled-gen)
     (aio-fs-request-operation who handle path start gen canceled-gen #f)]
    [(who handle path start gen canceled-gen serial-resource)
     ;; Callbacks receive ss so all mutable state belongs to this perform.
     (let ([token (list 'fs-request-operation)])
       (aio-make-operation
         (lambda (ss) #f)
         (lambda (ss deliver)
           (when serial-resource
             (aio-check-serial-scope! who serial-resource))
           (let ([st-box (box #f)] [id-box (box #f)]
                 [started? (box #f)] [entry-box (box #f)]
                 [canceled? (box #f)])
             ($async-sync-slot-set! ss token
               (vector st-box id-box started? entry-box canceled?))
             (letrec ([release!
                       (lambda ()
                         (when serial-resource
                           (let ([next
                                  (with-mutex (aio-serial-mutex serial-resource)
                                    (let ([entry
                                           (aio-queue-pop!
                                             (aio-serial-queue serial-resource))])
                                      (if (not entry)
                                          (begin
                                            (aio-serial-busy?-set! serial-resource #f)
                                            #f)
                                          (begin
                                            (set-box! (cadr entry) #t)
                                            (caddr entry)))))])
                             (when next (next)))))]
                      [finish-normal
                       (lambda (status aux)
                         (dynamic-wind
                           (lambda () (void))
                           (lambda ()
                             (if serial-resource
                                 (with-mutex (aio-serial-mutex serial-resource)
                                   (gen ss status aux))
                                 (gen ss status aux)))
                           release!))]
                      [finish-canceled
                       (lambda (status aux)
                         (dynamic-wind
                           (lambda () (void))
                           (lambda ()
                             (if serial-resource
                                 (with-mutex (aio-serial-mutex serial-resource)
                                   (canceled-gen ss status aux))
                                 (canceled-gen ss status aux)))
                           release!))]
                      [submit
                     (lambda ()
                       (let ([st (if serial-resource
                                     (aio-serial-state serial-resource)
                                     (aio-ensure-state! who))])
                       (define submit-native!
                         (lambda ()
                           (let* ([id (aio-next-id st)]
                                  [r (start ss st id)])
                             (aio-atomic-box-set-once! id-box id)
                             (if (< r 0)
                                 (cons 'error r)
                                 (begin
                                   (aio-register-request! st id
                                     (make-aio-req 'fs handle deliver r
                                       (aio-fs-finish finish-normal
                                         finish-canceled)
                                       #f))
                                   (when (or (aio-atomic-box-ref canceled?)
                                             (aio-waiter-dead? ss))
                                     (aio-cancel-request! st id))
                                   (cons 'submitted id))))))
                       (define run!
                         (lambda ()
                           (let ([result
                                  (guard (c [else (cons 'exception c)])
                                    (if serial-resource
                                        (with-mutex
                                          (aio-serial-mutex serial-resource)
                                          (if (or (aio-atomic-box-ref canceled?)
                                                  (aio-waiter-dead? ss))
                                              '(withdrawn)
                                              (submit-native!)))
                                        (if (or (aio-atomic-box-ref canceled?)
                                                (aio-waiter-dead? ss))
                                            '(withdrawn)
                                            (submit-native!))))])
                             (case (car result)
                               [(submitted) (void)]
                               [(withdrawn) (release!)]
                               [(error)
                                (release!)
                                (deliver
                                  (cons 'raise
                                    (aio-io-condition who handle path
                                      (cdr result))))]
                               [else
                                (release!)
                                (deliver (cons 'raise (cdr result)))]))))
                         (aio-atomic-box-set-once! st-box st)
                         (if (aio-run-on-owner! st run!)
                             (list 'fs who)
                             (begin
                               (release!)
                               (deliver
                                 (cons 'raise
                                   (aio-io-condition who handle path 'closed)))
                               #f))))])
             (if serial-resource
                 (let ([entry (list ss started? submit)] [start-now? #f])
                   (set-box! entry-box entry)
                   (with-mutex (aio-serial-mutex serial-resource)
                     (if (aio-serial-busy? serial-resource)
                         (set-box! entry-box
                           (aio-queue-push!
                             (aio-serial-queue serial-resource) entry))
                         (begin
                           (aio-serial-busy?-set! serial-resource #t)
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
                     [nack-started? (not serial-resource)])
                 (aio-atomic-box-flag! (vector-ref attempt 4))
                 (when serial-resource
                   (with-mutex (aio-serial-mutex serial-resource)
                     (if (unbox started?)
                         (set! nack-started? #t)
                         (let ([node (unbox entry-box)])
                           (when node
                             (aio-queue-remove!
                               (aio-serial-queue serial-resource) node))))))
                 (when nack-started?
                   (let ([st (aio-atomic-box-ref st-box)]
                         [id (aio-atomic-box-ref id-box)])
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
       (aio-fs-request-operation 'open #f path
         (lambda (ss st id)
           ($async-sync-slot-set! ss token st)
           (aio-fs-open (aio-state-loop st) path bits mode id))
         (lambda (ss status aux)
           (if (>= status 0)
               (let ([st ($async-sync-slot-ref ss token #f)])
                 (let ([f
                        (make-async-file% status path st #f
                          (if (fxlogtest bits 16) -1 0) ; append: track end lazily
                          #f (make-mutex) #f (make-aio-queue))])
                   (cons 'values (list (aio-register-file! st f)))))
               (cons 'raise (aio-io-condition 'open #f path status))))
         (lambda (ss status aux)
           (when (>= status 0)
             (aio-fs-close-now status)))))]))

(define aio-check-file/raw
  (lambda (who f)
    (when (async-file-closed? f)
      (raise (aio-io-condition who f (async-file-path f) 'closed)))))

(define aio-check-file
  (lambda (who f)
    (unless (%async-file? f)
      ($oops who "~s is not an async file" f))
    (with-mutex (async-file-mutex f)
      (aio-check-file/raw who f))))

(define aio-check-file-unowned
  (lambda (who f)
    (unless (%async-file? f)
      ($oops who "~s is not an async file" f))
    (with-mutex (async-file-mutex f)
      (aio-check-file/raw who f)
      (when (async-file-port-owned? f)
        ($oops who "async file ownership has been transferred to a port")))))

(define aio-check-file-unowned/raw
  (lambda (who f)
    (aio-check-file/raw who f)
    (when (async-file-port-owned? f)
      ($oops who "async file ownership has been transferred to a port"))))

(define aio-check-file-scope!
  (lambda (who f)
    (let ([sched (current-async-scheduler)])
      (unless (and sched
                   (eq? ($async-scheduler-group-token sched)
                        ($async-scheduler-group-token
                          (aio-state-owner (async-file-state f)))))
        ($oops who "async file belongs to another scheduler group")))))

(define aio-claim-file-for-port!
  (lambda (who f)
    (unless (%async-file? f)
      ($oops who "~s is not an async file" f))
    (aio-check-file-scope! who f)
    (with-mutex (async-file-mutex f)
      (aio-check-file/raw who f)
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
            (aio-check-file/raw 'file-read-operation f)
            (aio-check-file-unowned/raw 'file-read-operation f))
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
            (aio-check-file/raw 'file-write-operation f)
            (aio-check-file-unowned/raw 'file-write-operation f))
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
              (cons 'values '()))
            (cons 'raise
              (aio-io-condition 'close f (async-file-path f) status))))
      (lambda (ss status aux)
        (when (fx= status 0)
          (async-file-closed?-set! f #t)
          (aio-unregister-file! f)))
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
    (if (with-mutex (async-file-mutex f)
          (fx>= (async-file-offset f) 0))
        (values
          (lambda ()
            (with-mutex (async-file-mutex f)
              (aio-check-file/raw 'port-position f)
              (async-file-offset f)))
          (lambda (position)
            (with-mutex (async-file-mutex f)
              (aio-check-file/raw 'set-port-position! f)
              (async-file-offset-set! f position))))
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
           (aio-check-file-scope! 'file-stat-operation target)
           (aio-check-file-unowned/raw 'file-stat-operation target)
           (aio-fs-fstat (aio-state-loop st) (async-file-fd target) id))
         (lambda (ss status aux)
           (if (fx= status 0)
               (cons 'values (list (aio-stat-alist aux)))
               (cons 'raise
                 (aio-io-condition 'stat target (async-file-path target) status))))
         (lambda (ss status aux) (void))
         target)]
      [else
       ($oops 'file-stat-operation "~s is not a path or async file" target)])))

(define aio-check-path
  (lambda (who path)
    (unless (string? path) ($oops who "~s is not a string" path))))

(define aio-check-mode
  (lambda (who mode)
    (unless (and (fixnum? mode) (fx>= mode 0) (fx<= mode #o7777))
      ($oops who "~s is not a valid file mode" mode))))

(define aio-fs-void-operation
  (lambda (who handle path start . maybe-file)
    (aio-fs-request-operation who handle path start
      (lambda (ss status aux)
        (if (fx>= status 0)
            (cons 'values '())
            (cons 'raise (aio-io-condition who handle path status))))
      (lambda (ss status aux) (void))
      (and (pair? maybe-file) (car maybe-file)))))

(define aio-fs-result-cstring
  (lambda (aux length copy who path)
    (let ([n (length aux)])
      (if (fx< n 0)
          (raise (aio-io-condition who #f path n))
          (let ([bv (make-bytevector (fx+ n 1))])
            (let ([r (copy aux bv (fx+ n 1))])
              (if (fx< r 0)
                  (raise (aio-io-condition who #f path r))
                  (bv->cstring bv))))))))

(define %file-delete-operation
  (lambda (path)
    (aio-check-path 'file-delete-operation path)
    (aio-fs-void-operation 'delete #f path
      (lambda (ss st id) (aio-fs-unlink (aio-state-loop st) path id)))))

(define %file-rename-operation
  (lambda (old new)
    (aio-check-path 'file-rename-operation old)
    (aio-check-path 'file-rename-operation new)
    (aio-fs-void-operation 'rename #f old
      (lambda (ss st id)
        (aio-fs-rename (aio-state-loop st) old new id)))))

(define %directory-create-operation
  (case-lambda
    [(path) (%directory-create-operation path #o755)]
    [(path mode)
     (aio-check-path 'directory-create-operation path)
     (aio-check-mode 'directory-create-operation mode)
     (aio-fs-void-operation 'mkdir #f path
       (lambda (ss st id)
         (aio-fs-mkdir (aio-state-loop st) path mode id)))]))

(define %directory-delete-operation
  (lambda (path)
    (aio-check-path 'directory-delete-operation path)
    (aio-fs-void-operation 'rmdir #f path
      (lambda (ss st id) (aio-fs-rmdir (aio-state-loop st) path id)))))

(define aio-copy-flag-bits
  (lambda (who flags)
    (unless (and (list? flags) (for-all symbol? flags))
      ($oops who "~s is not a list of copy flags" flags))
    (fold-left
      (lambda (bits flag)
        (case flag
          [(exclusive) (fxlogior bits 1)]
          [(clone) (fxlogior bits 2)]
          [(clone-force) (fxlogior bits 4)]
          [else ($oops who "~s is not a copy flag" flag)]))
      0 flags)))

(define %file-copy-operation
  (case-lambda
    [(from to) (%file-copy-operation from to '())]
    [(from to flags)
     (aio-check-path 'file-copy-operation from)
     (aio-check-path 'file-copy-operation to)
     (let ([bits (aio-copy-flag-bits 'file-copy-operation flags)])
       (aio-fs-void-operation 'copy #f from
         (lambda (ss st id)
           (aio-fs-copyfile (aio-state-loop st) from to bits id))))]))

(define %temporary-directory-create-operation
  (lambda (pattern)
    (aio-check-path 'temporary-directory-create-operation pattern)
    (aio-fs-request-operation 'mkdtemp #f pattern
      (lambda (ss st id) (aio-fs-mkdtemp (aio-state-loop st) pattern id))
      (lambda (ss status aux)
        (if (fx= status 0)
            (cons 'values
              (list (aio-fs-result-cstring aux aio-fs-result-path-length
                      aio-fs-result-path-copy 'mkdtemp pattern)))
            (cons 'raise (aio-io-condition 'mkdtemp #f pattern status)))))))

(define %temporary-file-open-operation
  (lambda (pattern)
    (aio-check-path 'temporary-file-open-operation pattern)
    (let ([token (list 'temporary-file-open-operation)])
      (aio-fs-request-operation 'mkstemp #f pattern
        (lambda (ss st id)
          ($async-sync-slot-set! ss token st)
          (aio-fs-mkstemp (aio-state-loop st) pattern id))
        (lambda (ss status aux)
          (if (fx>= status 0)
              (let* ([st ($async-sync-slot-ref ss token #f)]
                     [path (aio-fs-result-cstring aux
                             aio-fs-result-path-length aio-fs-result-path-copy
                             'mkstemp pattern)]
                     [f (make-async-file% status path st #f 0 #f
                          (make-mutex) #f (make-aio-queue))])
                (cons 'values (list (aio-register-file! st f) path)))
              (cons 'raise (aio-io-condition 'mkstemp #f pattern status))))
        (lambda (ss status aux)
          (when (fx>= status 0) (aio-fs-close-now status)))))))

(define aio-dirent-type
  (lambda (n)
    (case n
      [(1) 'unknown]
      [(2) 'file]
      [(3) 'directory]
      [(4) 'link]
      [(5) 'fifo]
      [(6) 'socket]
      [(7) 'character-device]
      [(8) 'block-device]
      [else 'unknown])))

(define %directory-scan-operation
  (lambda (path)
    (aio-check-path 'directory-scan-operation path)
    (aio-fs-request-operation 'scandir #f path
      (lambda (ss st id) (aio-fs-scandir (aio-state-loop st) path id))
      (lambda (ss status aux)
        (if (fx>= status 0)
            (let ([buf (make-bytevector 4097)])
              (let loop ([entries '()])
                (let ([type (aio-fs-scandir-next aux buf 4097)])
                  (cond
                    [(fx= type 0) (cons 'values (list (reverse entries)))]
                    [(fx< type 0)
                     (cons 'raise (aio-io-condition 'scandir #f path type))]
                    [else
                     (loop (cons (cons (bv->cstring buf)
                                      (aio-dirent-type type))
                                  entries))]))))
            (cons 'raise (aio-io-condition 'scandir #f path status)))))))

(define aio-check-directory/raw
  (lambda (who directory)
    (when (async-directory-closed? directory)
      (raise
        (aio-io-condition who directory (async-directory-path directory)
          'closed)))))

(define aio-check-directory
  (lambda (who directory)
    (unless (%async-directory? directory)
      ($oops who "~s is not an async directory" directory))
    (with-mutex (async-directory-mutex directory)
      (aio-check-directory/raw who directory))))

(define %directory-open-operation
  (lambda (path)
    (aio-check-path 'directory-open-operation path)
    (let ([token (list 'directory-open-operation)])
      (aio-fs-request-operation 'opendir #f path
        (lambda (ss st id)
          ($async-sync-slot-set! ss token st)
          (aio-fs-opendir (aio-state-loop st) path id))
        (lambda (ss status aux)
          (if (fx>= status 0)
              (let* ([st ($async-sync-slot-ref ss token #f)]
                     [directory
                      (make-async-directory% (aio-fs-result-ptr aux) path st
                        #f (make-mutex) #f (make-aio-queue))])
                (cons 'values
                  (list (aio-register-directory! st directory))))
              (cons 'raise (aio-io-condition 'opendir #f path status))))
        (lambda (ss status aux)
          (when (fx>= status 0)
            (aio-fs-closedir-now (aio-fs-result-ptr aux))))))))

(define %directory-read-operation
  (case-lambda
    [(directory) (%directory-read-operation directory 64)]
    [(directory count)
     (aio-check-directory 'directory-read-operation directory)
     (unless (and (fixnum? count) (fx> count 0))
       ($oops 'directory-read-operation
         "~s is not a positive entry count" count))
     (aio-fs-request-operation 'readdir directory
       (async-directory-path directory)
       (lambda (ss st id)
         (aio-check-directory/raw 'directory-read-operation directory)
         (aio-fs-readdir (aio-state-loop st)
           (async-directory-pointer directory) count id))
       (lambda (ss status aux)
         (if (fx>= status 0)
             (let ([buf (make-bytevector 4097)])
               (let loop ([i 0] [entries '()])
                 (if (fx= i status)
                     (cons 'values (list (reverse entries)))
                     (let ([type (aio-fs-readdir-entry aux i buf 4097)])
                       (if (fx< type 0)
                           (cons 'raise
                             (aio-io-condition 'readdir directory
                               (async-directory-path directory) type))
                           (loop (fx+ i 1)
                             (cons (cons (bv->cstring buf)
                                         (aio-dirent-type type))
                                   entries)))))))
             (cons 'raise
               (aio-io-condition 'readdir directory
                 (async-directory-path directory) status))))
       (lambda (ss status aux) (void))
       directory)]))

(define aio-directory-close-complete!
  (lambda (directory status)
    (when (fx>= status 0)
      (async-directory-closed?-set! directory #t)
      (aio-unregister-directory! directory))))

(define %directory-close-operation
  (lambda (directory)
    (aio-check-directory 'directory-close-operation directory)
    (aio-fs-request-operation 'closedir directory
      (async-directory-path directory)
      (lambda (ss st id)
        (aio-check-directory/raw 'directory-close-operation directory)
        (aio-fs-closedir (aio-state-loop st)
          (async-directory-pointer directory) id))
      (lambda (ss status aux)
        (aio-directory-close-complete! directory status)
        (if (fx>= status 0)
            (cons 'values (list (void)))
            (cons 'raise
              (aio-io-condition 'closedir directory
                (async-directory-path directory) status))))
      (lambda (ss status aux)
        (aio-directory-close-complete! directory status))
      directory)))

(define %file-sync-operation
  (lambda (f data-only?)
    (aio-check-file-unowned (if data-only? 'file-data-sync-operation
                                'file-sync-operation) f)
    (let ([who (if data-only? 'fdatasync 'fsync)])
      (aio-fs-void-operation who f (async-file-path f)
        (lambda (ss st id)
          ((if data-only? aio-fs-fdatasync aio-fs-fsync)
           (aio-state-loop st) (async-file-fd f) id))
        f))))

(define %file-truncate-operation
  (lambda (f length)
    (aio-check-file-unowned 'file-truncate-operation f)
    (unless (and (integer? length) (exact? length) (>= length 0))
      ($oops 'file-truncate-operation "~s is not an exact nonnegative length"
        length))
    (aio-fs-void-operation 'truncate f (async-file-path f)
      (lambda (ss st id)
        (aio-fs-ftruncate (aio-state-loop st) (async-file-fd f) length id))
      f)))

(define %file-send-operation
  (lambda (output input offset length)
    (aio-check-file-unowned 'file-send-operation output)
    (aio-check-file-unowned 'file-send-operation input)
    (when (eq? output input)
      ($oops 'file-send-operation "input and output files are the same"))
    (unless (and (integer? offset) (exact? offset) (>= offset 0))
      ($oops 'file-send-operation "~s is not a nonnegative offset" offset))
    (unless (and (integer? length) (exact? length) (>= length 0))
      ($oops 'file-send-operation "~s is not a nonnegative length" length))
    (aio-fs-request-operation 'sendfile output (async-file-path input)
      (lambda (ss st id)
        (aio-check-file-scope! 'file-send-operation input)
        (with-mutex (async-file-mutex input)
          (aio-check-file-unowned/raw 'file-send-operation input)
          (aio-fs-sendfile (aio-state-loop st)
            (async-file-fd output) (async-file-fd input) offset length id)))
      (lambda (ss status aux)
        (if (fx>= status 0)
            (begin
              (aio-file-offset! output status)
              (cons 'values (list status)))
            (cons 'raise
              (aio-io-condition 'sendfile output (async-file-path input)
                status))))
      (lambda (ss status aux) (void))
      output)))

(define aio-access-mode-bits
  (lambda (who modes)
    (unless (and (list? modes) (for-all symbol? modes))
      ($oops who "~s is not a list of access modes" modes))
    (fold-left
      (lambda (bits mode)
        (case mode
          [(exists) bits]
          [(read) (fxlogior bits 1)]
          [(write) (fxlogior bits 2)]
          [(execute) (fxlogior bits 4)]
          [else ($oops who "~s is not an access mode" mode)]))
      0 modes)))

(define %file-access-operation
  (lambda (path modes)
    (aio-check-path 'file-access-operation path)
    (let ([bits (aio-access-mode-bits 'file-access-operation modes)])
      (aio-fs-request-operation 'access #f path
        (lambda (ss st id)
          (aio-fs-access (aio-state-loop st) path bits id))
        (lambda (ss status aux) (cons 'values (list (fx= status 0))))))))

(define %file-mode-set-operation
  (lambda (target mode)
    (aio-check-mode 'file-mode-set-operation mode)
    (cond
      [(string? target)
       (aio-fs-void-operation 'chmod #f target
         (lambda (ss st id)
           (aio-fs-chmod (aio-state-loop st) target mode id)))]
      [(%async-file? target)
       (aio-check-file-unowned 'file-mode-set-operation target)
       (aio-fs-void-operation 'fchmod target (async-file-path target)
         (lambda (ss st id)
           (aio-fs-fchmod (aio-state-loop st) (async-file-fd target) mode id))
         target)]
      [else ($oops 'file-mode-set-operation
              "~s is not a path or async file" target)])))

(define aio-check-time
  (lambda (who value)
    (unless (real? value) ($oops who "~s is not a real timestamp" value))
    (if (inexact? value) value (exact->inexact value))))

(define %file-times-set-operation
  (case-lambda
    [(target atime mtime) (%file-times-set-operation target atime mtime #t)]
    [(target atime mtime follow?)
     (let ([a (aio-check-time 'file-times-set-operation atime)]
           [m (aio-check-time 'file-times-set-operation mtime)])
       (cond
         [(string? target)
          (aio-fs-void-operation (if follow? 'utime 'lutime) #f target
            (lambda (ss st id)
              (aio-fs-utime (aio-state-loop st) target a m
                (if follow? 1 0) id)))]
         [(%async-file? target)
          (aio-check-file-unowned 'file-times-set-operation target)
          (aio-fs-void-operation 'futime target (async-file-path target)
            (lambda (ss st id)
              (aio-fs-futime (aio-state-loop st) (async-file-fd target)
                a m id))
            target)]
         [else ($oops 'file-times-set-operation
                 "~s is not a path or async file" target)]))]))

(define %file-lstat-operation
  (lambda (path)
    (aio-check-path 'file-lstat-operation path)
    (aio-fs-request-operation 'lstat #f path
      (lambda (ss st id) (aio-fs-lstat (aio-state-loop st) path id))
      (lambda (ss status aux)
        (if (fx= status 0)
            (cons 'values (list (aio-stat-alist aux)))
            (cons 'raise (aio-io-condition 'lstat #f path status)))))))

(define %file-link-operation
  (lambda (from to symbolic? flags)
    (aio-check-path 'file-link-operation from)
    (aio-check-path 'file-link-operation to)
    (let ([bits
           (fold-left
             (lambda (bits flag)
               (case flag
                 [(directory) (fxlogior bits 1)]
                 [(junction) (fxlogior bits 2)]
                 [else ($oops 'file-link-operation
                         "~s is not a symbolic-link flag" flag)]))
             0 flags)])
      (aio-fs-void-operation (if symbolic? 'symlink 'link) #f from
        (lambda (ss st id)
          (aio-fs-link (aio-state-loop st) from to
            (if symbolic? 1 0) bits id))))))

(define %file-read-link-operation
  (lambda (path realpath?)
    (aio-check-path 'file-read-link-operation path)
    (let ([who (if realpath? 'realpath 'readlink)])
      (aio-fs-request-operation who #f path
        (lambda (ss st id)
          (aio-fs-readlink (aio-state-loop st) path
            (if realpath? 1 0) id))
        (lambda (ss status aux)
          (if (fx= status 0)
              (cons 'values
                (list (aio-fs-result-cstring aux aio-fs-result-string-length
                        aio-fs-result-string-copy who path)))
              (cons 'raise (aio-io-condition who #f path status))))))))

(define %file-owner-set-operation
  (case-lambda
    [(target uid gid) (%file-owner-set-operation target uid gid #t)]
    [(target uid gid follow?)
     (unless (and (integer? uid) (exact? uid) (>= uid 0))
       ($oops 'file-owner-set-operation "~s is not a uid" uid))
     (unless (and (integer? gid) (exact? gid) (>= gid 0))
       ($oops 'file-owner-set-operation "~s is not a gid" gid))
     (cond
       [(string? target)
        (aio-fs-void-operation (if follow? 'chown 'lchown) #f target
          (lambda (ss st id)
            (aio-fs-chown (aio-state-loop st) target -1 uid gid
              (if follow? 0 2) id)))]
       [(%async-file? target)
        (aio-check-file-unowned 'file-owner-set-operation target)
        (aio-fs-void-operation 'fchown target (async-file-path target)
          (lambda (ss st id)
            (aio-fs-chown (aio-state-loop st) "" (async-file-fd target)
              uid gid 1 id))
          target)]
       [else ($oops 'file-owner-set-operation
               "~s is not a path or async file" target)])]))

(define aio-statfs-alist
  (lambda (aux)
    (define (field i) (aio-fs-statfs-field aux i))
    (list (cons 'type (field 0))
          (cons 'block-size (field 1))
          (cons 'blocks (field 2))
          (cons 'blocks-free (field 3))
          (cons 'blocks-available (field 4))
          (cons 'files (field 5))
          (cons 'files-free (field 6))
          (cons 'fragment-size (field 7)))))

(define %file-system-stat-operation
  (lambda (path)
    (aio-check-path 'file-system-stat-operation path)
    (aio-fs-request-operation 'statfs #f path
      (lambda (ss st id) (aio-fs-statfs (aio-state-loop st) path id))
      (lambda (ss status aux)
        (if (fx= status 0)
            (cons 'values (list (aio-statfs-alist aux)))
            (cons 'raise (aio-io-condition 'statfs #f path status)))))))
