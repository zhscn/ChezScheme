;;; asyncio.ms
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

(import (chezscheme async))
(import (chezscheme async operations))
(import (chezscheme async context))
(import (chezscheme async sync))
(import (chezscheme async io errors))
(import (chezscheme async io stream))
(import (chezscheme async io dns))
(import (chezscheme async io system))
(import (chezscheme async io poll))
(import (chezscheme async io process))
(import (chezscheme async io signal))
(import (chezscheme async io watch))
(import (chezscheme async io tty))
(import (chezscheme async io udp))
(import (chezscheme async io fs))

(define asyncio-temp-path
  (lambda (tag)
    (format "/tmp/chez-asyncio-~a-~a" (get-process-id) tag)))

(define asyncio-delete-if-present
  (lambda (path)
    (when (file-exists? path) (delete-file path))))

(define asyncio-test-burn
  (lambda (n)
    (let loop ([i 0])
      (unless (fx= i n)
        (loop (fx+ i 1))))))

(define asyncio-open-fd-count
  (lambda ()
    (and (file-exists? "/proc/self/fd")
         (length (directory-list "/proc/self/fd")))))

(mat asyncio

  ;; conditions and argument validation

  (let ([c (make-async-io-condition 'read 'handle "path" -1)])
    (and (async-io-condition? c)
         (eq? (async-io-condition-operation c) 'read)
         (eq? (async-io-condition-handle c) 'handle)
         (string=? (async-io-condition-path c) "path")
         (= (async-io-condition-code c) -1)))
  ;; The statically linked FFI table is published exactly once when unrelated
  ;; native threads make the first I/O-library call concurrently.
  (or (not (threaded?))
      (let ([mutex (make-mutex)]
            [condition (make-condition)]
            [start? #f]
            [results (make-vector 8 #f)])
        (let ([threads
               (vector-map
                 (lambda (i)
                   (fork-thread
                     (lambda ()
                       (with-mutex mutex
                         (let wait ()
                           (unless start?
                             (condition-wait condition mutex)
                             (wait))))
                       (vector-set! results i (async-io-error-name -1)))))
                 '#(0 1 2 3 4 5 6 7))])
          (with-mutex mutex
            (set! start? #t)
            (condition-broadcast condition))
          (vector-for-each thread-join threads)
          (for-all string? (vector->list results)))))
  (and (string? (async-io-error-name -1))
       (string? (async-io-error-message -1)))
  (error? (run-async (lambda () (tcp-listen "127.0.0.1" 65536))))
  (error? (run-async (lambda () (pipe-listen 17))))
  (error? (run-async (lambda () (stream-read 'not-a-stream))))
  (error? (run-async (lambda () (file-open "x" '(bogus)))))
  (error? (run-async (lambda () (file-open "x" '(read) -1))))
  (error? (run-async (lambda () (file-read 'not-a-file 1))))
  (error? (async-stream->binary-input-port 'not-a-stream))
  (error? (async-stream->binary-output-port 'not-a-stream))
  (error? (async-stream->binary-input/output-port 'not-a-stream))
  (error? (async-file->binary-input-port 'not-a-file))
  (error? (async-file->binary-output-port 'not-a-file))
  (error? (async-file->binary-input/output-port 'not-a-file))

  ;; filesystem requests preserve contents, offsets, and stat metadata

  (let ([path (asyncio-temp-path "file")])
    (asyncio-delete-if-present path)
    (let ((result
           (run-async
             (lambda ()
               (asyncio-test-burn 100000)
               (let ((out (file-open path '(write create truncate))))
                 (let ((n (file-write out (string->utf8 "hello async"))))
                   (file-close out)
                   (let ((in (file-open path '(read))))
                     (let* ((bv (file-read in 64))
                            (eof (file-read in 64))
                            (stat (file-stat in)))
                       (file-close in)
                       (list n (utf8->string bv) (eof-object? eof)
                             (cdr (assq 'size stat))))))))
             'preemption-ticks 1000)))
      (asyncio-delete-if-present path)
      (equal? result '(11 "hello async" #t 11))))

  ;; Implicit file positions are serialized across concurrent operations.
  (let ([path (asyncio-temp-path "concurrent-offset")])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([file (file-open path '(write create truncate))]
                      [a (spawn-task
                           (lambda ()
                             (file-write file (string->utf8 "aaaa"))))]
                      [b (spawn-task
                           (lambda ()
                             (file-write file (string->utf8 "bbbb"))))])
                 (task-join a)
                 (task-join b)
                 (file-close file)
                 (cdr (assq 'size (file-stat path))))))])
      (asyncio-delete-if-present path)
      (= result 8)))

  ;; Canceling an operation while it is queued does not submit it later.
  (let ([path (asyncio-temp-path "queued-cancel")]
        [payload (make-bytevector (* 1024 1024) 7)])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([file (file-open path '(write create truncate))]
                      [first (spawn-task (lambda () (file-write file payload)))]
                      [withdrawn
                       (spawn-task
                         (lambda ()
                           (file-write file (string->utf8 "late"))))])
                 (task-yield)
                 (task-cancel! withdrawn)
                 (guard (c [(async-cancellation-condition? c) (void)])
                   (task-join withdrawn))
                 (task-join first)
                 (file-close file)
                 (cdr (assq 'size (file-stat path))))))])
      (asyncio-delete-if-present path)
      (= result (bytevector-length payload))))

  ;; A successful open result is closed when cancellation wins before the
  ;; scheduler dispatches the completion.
  (let ([before (asyncio-open-fd-count)])
    (or (not before)
        (begin
          (run-async
            (lambda ()
              ;; Warm the libuv worker pool before opening the cancellation
              ;; window measured below.
              (file-stat "/dev/null")
              (do ([i 0 (fx+ i 1)]) ((fx= i 16))
                (let ([task
                       (spawn-task
                         (lambda () (file-open "/dev/null" '(read))))])
                  (task-yield)
                  (sleep (make-time 'time-duration 10000000 0))
                  (task-cancel! task)
                  (guard (c [(async-cancellation-condition? c) (void)])
                    (let ([file (task-join task)])
                      (file-close file)))))))
          (let ([after (asyncio-open-fd-count)])
            (and after (<= after (+ before 1)))))))

  ;; Scheduler shutdown closes files that were not explicitly closed.
  (let ([before (asyncio-open-fd-count)])
    (or (not before)
        (begin
          (run-async
            (lambda ()
              (file-open "/dev/null" '(read))
              (void)))
          (let ([after (asyncio-open-fd-count)])
            (and after (<= after (+ before 1)))))))

  ;; File adapters preserve port buffering, position, EOF, and ownership.

  (let ([path (asyncio-temp-path "file-port")]
        [payload (string->utf8 "port-backed async file")])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([file (file-open path '(read write create truncate))]
                      [port (async-file->binary-input/output-port file)]
                      [owned?
                       (guard (c [else #t])
                         (async-file->binary-input-port file)
                         #f)])
                 (put-bytevector port payload)
                 (flush-output-port port)
                 (let ([end (port-position port)])
                   (set-port-position! port 0)
                   (let* ([contents (get-bytevector-n port
                                      (bytevector-length payload))]
                          [eof (get-u8 port)])
                     (close-port port)
                     (list owned? end contents (eof-object? eof)
                       (port-closed? port)
                       (guard (c [(async-io-condition? c)
                                  (async-io-condition-code c)])
                         (file-read file 1))))))))])
      (asyncio-delete-if-present path)
      (equal? result
        (list #t (bytevector-length payload) payload #t #t 'closed))))

  ;; Operations created before ownership transfer are rejected when they run.
  (let ([path (asyncio-temp-path "file-operation-owner")])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([file (file-open path '(write create truncate))]
                      [op (file-write-operation file (bytevector 1))]
                      [port (async-file->binary-output-port file)]
                      [rejected?
                       (guard (c [else #t])
                         (perform-operation op)
                         #f)])
                 (close-port port)
                 rejected?)))])
      (asyncio-delete-if-present path)
      result))

  ;; Extended filesystem requests preserve native result ownership and expose
  ;; both operation and direct forms.
  (let ([prefix (asyncio-temp-path "fs-XXXXXX")])
    (let ([ok
           (run-async
             (lambda ()
               (let* ([dir (temporary-directory-create prefix)]
                      [path (string-append dir "/source")]
                      [copy (string-append dir "/copy")]
                      [hard (string-append dir "/hard")]
                      [sym (string-append dir "/sym")]
                      [file (file-open path '(read write create exclusive))])
                 (file-write file (string->utf8 "abcdef"))
                 (file-sync file)
                 (file-data-sync file)
                 (file-truncate file 3)
                 (file-mode-set! file #o600)
                 (file-times-set! file 1000 1001)
                 (let ([stat (file-stat file)])
                   (file-close file)
                   (file-copy path copy '(exclusive))
                   (file-link path hard)
                   (file-symbolic-link path sym)
                   (let* ([lstat (file-lstat sym)]
                          [target (file-read-link sym)]
                          [real (file-real-path sym)]
                          [entries (directory-scan dir)]
                          [fs (file-system-stat dir)]
                          [access? (file-access? path '(read write))]
                          [names (map car entries)])
                     (file-delete sym)
                     (file-delete hard)
                     (file-delete copy)
                     (file-delete path)
                     (directory-delete dir)
                     (and (= (cdr (assq 'size stat)) 3)
                          (not (= (fxlogand (cdr (assq 'mode lstat)) #o170000)
                                  #o100000))
                          (string=? target path)
                          (string=? real path)
                          access?
                          (for-all (lambda (name) (member name names))
                            '("source" "copy" "hard" "sym"))
                          (positive? (cdr (assq 'block-size fs)))))))))])
      ok))

  (let ([pattern (asyncio-temp-path "mkstemp-XXXXXX")])
    (run-async
      (lambda ()
        (let-values ([(file path)
                      (perform-operation
                        (temporary-file-open-operation pattern))])
          (let ([ok (and (async-file? file) (string? path)
                         (file-access? path '(exists)))])
            (perform-operation (file-close-operation file))
            (perform-operation (file-delete-operation path))
            ok)))))

  ;; Streaming directory handles read bounded batches and close explicitly.
  (let ([path (asyncio-temp-path "directory-handle")])
    (when (file-exists? path) (delete-directory path))
    (run-async
      (lambda ()
        (directory-create path)
        (for-each
          (lambda (name)
            (let ([f (file-open (string-append path "/" name)
                       '(write create truncate))])
              (file-close f)))
          '("a" "b" "c"))
        (let ([directory (directory-open path)])
          (let loop ([names '()])
            (let ([batch (directory-read directory 2)])
              (if (null? batch)
                  (begin
                    (directory-close directory)
                    (for-each
                      (lambda (name) (file-delete (string-append path "/" name)))
                      '("a" "b" "c"))
                    (directory-delete path)
                    (for-all (lambda (name) (and (member name names) #t))
                      '("a" "b" "c")))
                  (loop (append (map car batch) names)))))))))

  ;; sendfile keeps both descriptors alive until libuv completes the request.
  (let ([source (asyncio-temp-path "sendfile-source")]
        [target (asyncio-temp-path "sendfile-target")])
    (asyncio-delete-if-present source)
    (asyncio-delete-if-present target)
    (let ([result
           (run-async
             (lambda ()
               (let ([input (file-open source '(read write create truncate))]
                     [output (file-open target '(read write create truncate))])
                 (file-write input (string->utf8 "0123456789"))
                 (let ([sent (file-send output input 2 5)])
                   (file-close input)
                   (file-close output)
                   (let ([check (file-open target '(read))])
                     (let ([contents (utf8->string (file-read check 16))])
                       (file-close check)
                       (list sent contents)))))))])
      (asyncio-delete-if-present source)
      (asyncio-delete-if-present target)
      (equal? result '(5 "23456"))))

  ;; reverse DNS, random data, system information, and loop metrics.
  (run-async
    (lambda ()
      (let-values ([(host service)
                    (dns-reverse "127.0.0.1" 80
                      '(numeric-host numeric-service))])
        (let ([memory (system-memory-info)]
              [process (system-process-info)]
              [paths (system-path-info)]
              [uname (system-uname)]
              [usage (system-resource-usage)]
              [metrics (async-loop-metrics)])
          (and (string=? host "127.0.0.1")
               (string=? service "80")
               (= (bytevector-length (random-bytevector 32)) 32)
               (positive? (system-high-resolution-time))
               (real? (system-uptime))
               (= (length (system-load-average)) 3)
               (pair? (assq 'total memory))
               (pair? (assq 'pid process))
               (pair? (assq 'hostname paths))
               (pair? (assq 'system uname))
               (pair? (system-cpu-info))
               (list? (system-interface-info))
               (pair? (assq 'user-time usage))
               (pair? (assq 'loop-count metrics)))))))

  ;; A completion burst larger than one dispatch batch remains lossless.
  (equal? (run-async
            (lambda ()
              (let ([tasks
                     (let loop ([i 0] [tasks '()])
                       (if (fx= i 320)
                           tasks
                           (loop (fx+ i 1)
                             (cons
                               (spawn-task
                                 (lambda ()
                                   (bytevector-length (random-bytevector 8))))
                               tasks))))])
                (apply + (map task-join tasks)))))
    2560)

  ;; child stdio pipes use the ordinary stream protocol.
  (run-async
    (lambda ()
      (let-values ([(child stdin stdout stderr)
                    (process-spawn "/bin/sh"
                      '("-c" "printf process-stdout; printf process-stderr >&2"))])
        (stream-close stdin)
        (let ([out (utf8->string (stream-read stdout))]
              [err (utf8->string (stream-read stderr))])
          (let-values ([(status signal) (process-wait child)])
            (stream-close stdout)
            (stream-close stderr)
            (process-close child)
            (and (string=? out "process-stdout")
                 (string=? err "process-stderr")
                 (= status 0)
                 (= signal 0)))))))

  ;; signal watchers deliver exactly one awaited occurrence.
  (run-async
    (lambda ()
      (let* ([watcher (signal-open 10)]
             [sender
              (spawn-task
                (lambda ()
                  (async-sleep 0.01)
                  (process-id-kill (get-process-id) 10)))])
        (let ([received (signal-receive watcher)])
          (task-join sender)
          (signal-close watcher)
          (= received 10)))))

  ;; filesystem event and polling watchers report mutations.
  (let ([path (asyncio-temp-path "watch")])
    (asyncio-delete-if-present path)
    (mkdir path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([event (fs-event-open path)]
                      [changed (string-append path "/changed")]
                      [writer
                       (spawn-task
                         (lambda ()
                           (async-sleep 0.01)
                           (let ([f (file-open changed
                                      '(write create truncate))])
                             (file-write f (string->utf8 "x"))
                             (file-close f))))])
                 (let-values ([(name events) (fs-event-receive event)])
                   (task-join writer)
                   (fs-event-close event)
                   (file-delete changed)
                   (and (string? name)
                        (or (memq 'rename events) (memq 'change events))
                        #t)))))])
      (delete-directory path)
      result))

  (let ([path (asyncio-temp-path "fs-poll")])
    (asyncio-delete-if-present path)
    (call-with-output-file path
      (lambda (p) (display "x" p))
      'replace)
    (let ([result
           (run-async
             (lambda ()
               (let* ([poll (fs-poll-open path 10)]
                      [writer
                       (spawn-task
                         (lambda ()
                           (async-sleep 0.03)
                           (let ([f (file-open path '(write truncate))])
                             (file-write f (string->utf8 "xy"))
                             (file-close f))))])
                 (let-values ([(old current) (fs-poll-receive poll)])
                   (task-join writer)
                   (fs-poll-close poll)
                   (and (= (cdr (assq 'size old)) 1)
                        (= (cdr (assq 'size current)) 2))))))])
      (asyncio-delete-if-present path)
      result))

  ;; A process pipe supplies a pollable descriptor without transferring it.
  (let-values ([(to-stdin from-stdout from-stderr pid)
                (open-process-ports "/bin/true")])
    (let ([result
           (run-async
             (lambda ()
               (let ([poll (fd-poll-open
                             (port-file-descriptor from-stdout))])
                 (let ([events (fd-poll poll '(readable disconnect))])
                   (fd-poll-close poll)
                   (pair? events)))))])
      (close-output-port to-stdin)
      (close-input-port from-stdout)
      (close-input-port from-stderr)
      result))

  ;; TTY entry points validate descriptors even without a controlling TTY.
  (and (guard (c [else #t])
         (run-async (lambda () (tty-open -1)))
         #f)
       (guard (c [(async-io-condition? c) #t])
         (and (memq (tty-virtual-terminal-state) '(supported unsupported))
              #t)))

  ;; filesystem failures retain operation and path information

  (let ([path (asyncio-temp-path "missing")])
    (asyncio-delete-if-present path)
    (equal?
      (run-async
        (lambda ()
          (guard (c [(async-io-condition? c)
                     (list (async-io-condition-operation c)
                           (async-io-condition-path c)
                           (integer? (async-io-condition-code c)))])
            (file-open path '(read)))))
      (list 'open path #t)))

  ;; name resolution is delivered on the scheduler thread

  (let ([answers (run-async (lambda () (dns-lookup "localhost" "80")))])
    (and (pair? answers)
         (for-all
           (lambda (answer)
             (and (string? (car answer))
                  (= (cadr answer) 80)
                  (and (memv (caddr answer) '(4 6)) #t)))
           answers)))

  ;; UDP preserves datagram boundaries and reports numeric peer addresses.
  (run-async
    (lambda ()
      (let ([server (udp-open "127.0.0.1" 0)]
            [client (udp-open "127.0.0.1" 0)])
        (let-values ([(host port family) (udp-local-address server)])
          (udp-connect! client host port)
          (udp-option-set! client 'ttl 64)
          (let ([sent (perform-operation
                        (udp-send-operation client (string->utf8 "ping")))])
            (let-values ([(bv peer peer-port peer-family)
                          (perform-operation
                            (udp-receive-operation server))])
              (udp-disconnect! client)
              (udp-close client)
              (udp-close server)
              (and (= sent 4)
                   (string=? (utf8->string bv) "ping")
                   (= family 4)
                   (= peer-family 4)
                   (positive? peer-port))))))))

  ;; Each scheduler owns its libuv loop, and I/O completion resumes a task on
  ;; the scheduler thread where that task began executing.
  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let ([tasks (make-vector 2)]
                  [mutex (make-mutex)]
                  [condition (make-condition)]
                  [started 0])
              (do ([i 0 (fx+ i 1)]) ((fx= i 2))
                (vector-set! tasks i
                  (spawn-task
                    (lambda ()
                      (let ([scheduler (current-async-scheduler)]
                            [thread-id (get-thread-id)])
                        (with-mutex mutex
                          (set! started (fx+ started 1))
                          (when (fx= started 2)
                            (condition-broadcast condition))
                          (let loop ()
                            (unless (fx= started 2)
                              (condition-wait condition mutex)
                              (loop))))
                        (let ([answers (dns-lookup "localhost" "80")])
                          (list (pair? answers)
                                (eq? scheduler (current-async-scheduler))
                                (fx= thread-id (get-thread-id))))))
                    'migratable? #t)))
              (list (task-join (vector-ref tasks 0))
                    (task-join (vector-ref tasks 1)))))
          'parallelism 2)
        '((#t #t #t) (#t #t #t))))

  ;; File requests are routed to the file's loop while their fibers migrate.
  (or (not (threaded?))
      (let ([path (asyncio-temp-path "foreign-file-owner")])
        (asyncio-delete-if-present path)
        (let ([result
               (run-async
                 (lambda ()
                   (let* ([file (file-open path '(write create truncate))]
                          [mutex (make-mutex)]
                          [condition (make-condition)]
                          [started? #f]
                          [writer
                           (spawn-task
                             (lambda ()
                               (with-mutex mutex
                                 (set! started? #t)
                                 (condition-broadcast condition))
                               (file-write file (string->utf8 "routed"))
                               (file-close file)
                               #t)
                             'migratable? #t)]
                          [nudge
                           (spawn-task (lambda () (void)) 'migratable? #t)])
                     ;; Block scheduler 0 until scheduler 1 runs the writer.
                     (with-mutex mutex
                       (let loop ()
                         (unless started?
                           (condition-wait condition mutex)
                           (loop))))
                     (and (task-join writer)
                          (begin (task-join nudge) #t)
                          (= (cdr (assq 'size (file-stat path))) 6))))
                 'parallelism 2)])
          (asyncio-delete-if-present path)
          result)))

  ;; A request canceled from another scheduler is withdrawn by its loop owner.
  (or (not (threaded?))
      (run-async
        (lambda ()
          (let* ([mutex (make-mutex)]
                 [condition (make-condition)]
                 [started? #f]
                 [task
                  (spawn-task
                    (lambda ()
                      (with-mutex mutex
                        (set! started? #t)
                        (condition-broadcast condition))
                      (dns-lookup "localhost" "80"))
                    'migratable? #t)]
                 [nudge
                  (spawn-task (lambda () (void)) 'migratable? #t)])
            (with-mutex mutex
              (let loop ()
                (unless started?
                  (condition-wait condition mutex)
                  (loop))))
            (task-cancel! task)
            (let ([canceled?
                   (guard (c [(async-cancellation-condition? c) #t])
                     (task-join task)
                     #f)])
              (task-join nudge)
              canceled?)))
        'parallelism 2))

  ;; Handle requests made on another worker run on the handle's loop owner.
  (or (not (threaded?))
      (let ([path (asyncio-temp-path "foreign-owner")])
        (asyncio-delete-if-present path)
        (let ([result
               (run-async
                 (lambda ()
                   (let* ([listener (pipe-listen path)]
                          [owner (current-async-scheduler)]
                          [client (pipe-connect path)]
                          [server (tcp-accept listener)]
                          [mutex (make-mutex)]
                          [condition (make-condition)]
                          [started? #f]
                          [writer
                           (spawn-task
                             (lambda ()
                               (let ([foreign?
                                      (not (eq? (current-async-scheduler)
                                                owner))])
                                 (with-mutex mutex
                                   (set! started? #t)
                                   (condition-broadcast condition))
                                 (stream-write client (bytevector 1))
                                 foreign?))
                             'migratable? #t)]
                          [nudge
                           (spawn-task (lambda () (void)) 'migratable? #t)])
                     ;; Blocking the owner thread here forces the writer onto
                     ;; the other scheduler.  Joining it then releases the
                     ;; owner loop to process the routed stream request.
                     (with-mutex mutex
                       (let loop ()
                         (unless started?
                           (condition-wait condition mutex)
                           (loop))))
                     (let ([foreign? (task-join writer)]
                           [data (stream-read server)])
                       (task-join nudge)
                       (stream-close client)
                       (stream-close server)
                       (tcp-listener-close listener)
                       (and foreign? (= (bytevector-length data) 1)))))
                 'parallelism 2)])
          (asyncio-delete-if-present path)
          result)))

  ;; local-domain streams support accept, bidirectional I/O, and shutdown

  (let ([path (asyncio-temp-path "pipe")])
    (asyncio-delete-if-present path)
    (let ((result
           (run-async
             (lambda ()
               (let* ((listener (pipe-listen path))
                      (server
                        (spawn-task
                          (lambda ()
                            (let ((s (tcp-accept listener)))
                              (let ((request (utf8->string (stream-read s))))
                                (stream-write s (string->utf8 "pong"))
                                (stream-shutdown s)
                                (stream-close s)
                                request)))))
                      (client (pipe-connect path)))
                 (stream-write client (string->utf8 "ping"))
                 (let ((reply (utf8->string (stream-read client))))
                   (stream-close client)
                   (tcp-listener-close listener)
                   (list (task-join server) reply)))))))
      (asyncio-delete-if-present path)
      (equal? result '("ping" "pong"))))

  ;; Duplex stream ports avoid read-ahead, flush before close, and translate
  ;; stream EOF into port EOF.

  (let ([path (asyncio-temp-path "duplex-port")])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([listener (pipe-listen path)]
                      [server-task
                       (spawn-task
                         (lambda ()
                           (let* ([stream (tcp-accept listener)]
                                  [port
                                   (async-stream->binary-input/output-port
                                     stream)]
                                  [request (get-bytevector-n port 4)])
                             (put-bytevector port (string->utf8 "reply"))
                             (close-port port)
                             (list (utf8->string request)
                                   (port-closed? port)
                                   (stream-closed? stream)))))]
                      [client (pipe-connect path)]
                      [port
                       (async-stream->binary-input/output-port client)]
                      [owned?
                       (guard (c [else #t])
                         (async-stream->binary-input-port client)
                         #f)])
                 (put-bytevector port (string->utf8 "ping"))
                 (flush-output-port port)
                 (let ([reply (get-bytevector-all port)])
                   (close-port port)
                   (tcp-listener-close listener)
                   (list owned? (task-join server-task)
                         (utf8->string reply)
                         (port-closed? port))))))])
      (asyncio-delete-if-present path)
      (equal? result '(#t ("ping" #t #t) "reply" #t))))

  ;; Stream operations recheck ownership when performed, not only when they
  ;; are constructed.
  (let ([path (asyncio-temp-path "stream-operation-owner")])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([listener (pipe-listen path)]
                      [connector
                       (spawn-task (lambda () (pipe-connect path)))]
                      [server (tcp-accept listener)]
                      [client (task-join connector)]
                      [op (stream-read-operation server)]
                      [port (async-stream->binary-input-port server)]
                      [rejected?
                       (guard (c [else #t])
                         (perform-operation op)
                         #f)])
                 (close-port port)
                 (stream-close client)
                 (tcp-listener-close listener)
                 rejected?)))])
      (asyncio-delete-if-present path)
      result))

  ;; A stream input port retains a libuv read chunk that is larger than the
  ;; destination supplied by a particular port read.

  (let ([path (asyncio-temp-path "input-port")]
        [payload (make-bytevector 8192 37)])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([listener (pipe-listen path)]
                      [writer
                       (spawn-task
                         (lambda ()
                           (let* ([stream (tcp-accept listener)]
                                  [port
                                   (async-stream->binary-output-port stream)])
                             (put-bytevector port payload)
                             (close-port port))))]
                      [client (pipe-connect path)]
                      [port (async-stream->binary-input-port client)]
                      [prefix (get-bytevector-n port 3)]
                      [suffix (get-bytevector-all port)])
                 (close-port port)
                 (task-join writer)
                 (tcp-listener-close listener)
                 (list prefix suffix))))])
      (asyncio-delete-if-present path)
      (equal? result
        (list (make-bytevector 3 37) (make-bytevector 8189 37)))))

  ;; Cancellation propagates through a port read and leaves the next byte for
  ;; a later read from the same port.

  (let ([path (asyncio-temp-path "port-cancel")])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([listener (pipe-listen path)]
                      [connector
                       (spawn-task (lambda () (pipe-connect path)))]
                      [server (tcp-accept listener)]
                      [client (task-join connector)]
                      [port (async-stream->binary-input-port server)]
                      [reader
                       (spawn-task
                         (lambda ()
                           (guard (c [(async-cancellation-condition? c)
                                      'canceled])
                             (get-u8 port))))])
                 (task-yield)
                 (task-cancel! reader)
                 (let ([canceled (task-join reader)])
                   (stream-write client (bytevector 91))
                   (let ([next (get-u8 port)])
                     (close-port port)
                     (stream-close client)
                     (tcp-listener-close listener)
                     (list canceled next))))))])
      (asyncio-delete-if-present path)
      (equal? result '(canceled 91))))

  ;; closing a stream wakes a blocked reader exactly once

  (let ([path (asyncio-temp-path "close")])
    (asyncio-delete-if-present path)
    (let ((result
           (run-async
             (lambda ()
               (let* ((listener (pipe-listen path))
                      (connector
                        (spawn-task (lambda () (pipe-connect path))))
                      (server (tcp-accept listener))
                      (client (task-join connector))
                      (reader
                        (spawn-task
                          (lambda ()
                            (guard (c [(async-io-condition? c)
                                       (async-io-condition-code c)])
                              (stream-read server))))))
                 (task-yield)
                 (stream-close server)
                 (let ((code (task-join reader)))
                   (stream-close client)
                   (tcp-listener-close listener)
                   code))))))
      (asyncio-delete-if-present path)
      (eq? result 'closed)))

  ;; canceling a read disarms it without consuming a later read

  (let ([path (asyncio-temp-path "cancel")])
    (asyncio-delete-if-present path)
    (let ((result
           (run-async
             (lambda ()
               (let* ((listener (pipe-listen path))
                      (connector
                        (spawn-task (lambda () (pipe-connect path))))
                      (server (tcp-accept listener))
                      (client (task-join connector))
                      (reader
                        (spawn-task
                          (lambda ()
                            (guard (c [(async-cancellation-condition? c) 'canceled])
                              (stream-read server))))))
                 (task-yield)
                 (task-cancel! reader)
                 (let ((canceled (task-join reader)))
                   (stream-write client (string->utf8 "after-cancel"))
                   (let ((received (utf8->string (stream-read server))))
                     (stream-close server)
                     (stream-close client)
                     (tcp-listener-close listener)
                     (list canceled received))))))))
      (asyncio-delete-if-present path)
      (equal? result '(canceled "after-cancel"))))

  ;; Context cancellation uses the same nack path and leaves a later stream
  ;; read intact.

  (let ([path (asyncio-temp-path "context-cancel")])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([listener (pipe-listen path)]
                      [connector
                       (spawn-task (lambda () (pipe-connect path)))]
                      [server (tcp-accept listener)]
                      [client (task-join connector)]
                      [context (make-async-context)]
                      [started (make-future)]
                      [reader
                       (spawn-task
                         (lambda ()
                           (guard (c [(async-cancellation-condition? c)
                                      (async-cancellation-reason c)])
                             (future-fulfil! started)
                             (stream-read server)))
                         'context context)])
                 (future-get started)
                 (async-context-cancel! context 'context-canceled)
                 (let ([canceled (task-join reader)])
                   (stream-write client (string->utf8 "after-context"))
                   (let ([received (utf8->string (stream-read server))])
                     (stream-close server)
                     (stream-close client)
                     (tcp-listener-close listener)
                     (list canceled received))))))])
      (asyncio-delete-if-present path)
      (equal? result '(context-canceled "after-context"))))

  ;; Canceling a large stream waiter set unlinks each registration without
  ;; consuming the next live reader's bytes.
  (let ([path (asyncio-temp-path "cancel-fanout")])
    (asyncio-delete-if-present path)
    (let ([result
           (run-async
             (lambda ()
               (let* ([listener (pipe-listen path)]
                      [connector (spawn-task (lambda () (pipe-connect path)))]
                      [server (tcp-accept listener)]
                      [client (task-join connector)]
                      [started (make-async-wait-group 64)]
                      [readers
                       (let loop ([i 0] [tasks '()])
                         (if (fx= i 64)
                             tasks
                             (loop (fx+ i 1)
                               (cons
                                 (spawn-task
                                   (lambda ()
                                     (async-wait-group-done! started)
                                     (guard (c [else 'canceled])
                                       (stream-read server))))
                                 tasks))))])
                 (async-wait-group-wait started)
                 (task-yield)
                 (for-each (lambda (reader) (task-cancel! reader 'stop)) readers)
                 (for-each
                   (lambda (reader) (guard (c [else (void)]) (task-join reader)))
                   readers)
                 (stream-write client (string->utf8 "live"))
                 (let ([received (utf8->string (stream-read server))])
                   (stream-close server)
                   (stream-close client)
                   (tcp-listener-close listener)
                   received))))])
      (asyncio-delete-if-present path)
      (string=? result "live")))

  ;; TCP streams use the same operation and ownership protocol

  (let ([base (+ 20000 (modulo (get-process-id) 20000))])
    (run-async
      (lambda ()
        (let find-port ([port base] [remaining 32])
          (guard (c [(and (async-io-condition? c) (> remaining 0))
                     (find-port (+ port 1) (- remaining 1))])
            (let* ([listener (tcp-listen "127.0.0.1" port)]
                   [server
                     (spawn-task
                       (lambda ()
                         (let ([s (tcp-accept listener)])
                           (let ([request (utf8->string (stream-read s))])
                             (stream-write s (string->utf8 "tcp-reply"))
                             (stream-close s)
                             request))))]
                   [client (tcp-connect "127.0.0.1" port)])
              (stream-write client (string->utf8 "tcp-request"))
              (let ([reply (utf8->string (stream-read client))])
                (stream-close client)
                (tcp-listener-close listener)
                (and (string=? (task-join server) "tcp-request")
                     (string=? reply "tcp-reply")))))))))
)
