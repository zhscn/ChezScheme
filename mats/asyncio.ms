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
(import (chezscheme async io))

(define asyncio-temp-path
  (lambda (tag)
    (format "/tmp/chez-asyncio-~a-~a" (get-process-id) tag)))

(define asyncio-delete-if-present
  (lambda (path)
    (when (file-exists? path) (delete-file path))))

(mat asyncio

  ;; conditions and argument validation

  (let ([c (make-async-io-condition 'read 'handle "path" -1)])
    (and (async-io-condition? c)
         (eq? (async-io-condition-operation c) 'read)
         (eq? (async-io-condition-handle c) 'handle)
         (string=? (async-io-condition-path c) "path")
         (= (async-io-condition-code c) -1)))
  (and (string? (async-io-error-name -1))
       (string? (async-io-error-message -1)))
  (error? (run-async (lambda () (tcp-listen "127.0.0.1" 65536))))
  (error? (run-async (lambda () (pipe-listen 17))))
  (error? (run-async (lambda () (stream-read 'not-a-stream))))
  (error? (run-async (lambda () (file-open "x" '(bogus)))))
  (error? (run-async (lambda () (file-open "x" '(read) -1))))
  (error? (run-async (lambda () (file-read 'not-a-file 1))))

  ;; filesystem requests preserve contents, offsets, and stat metadata

  (let ([path (asyncio-temp-path "file")])
    (asyncio-delete-if-present path)
    (let ((result
           (run-async
             (lambda ()
               (let ((out (file-open path '(write create truncate))))
                 (let ((n (file-write out (string->utf8 "hello async"))))
                   (file-close out)
                   (let ((in (file-open path '(read))))
                     (let* ((bv (file-read in 64))
                            (eof (file-read in 64))
                            (stat (file-stat in)))
                       (file-close in)
                       (list n (utf8->string bv) (eof-object? eof)
                             (cdr (assq 'size stat)))))))))))
      (asyncio-delete-if-present path)
      (equal? result '(11 "hello async" #t 11))))

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
