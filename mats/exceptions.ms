;;; exceptions.ms
;;; Copyright 1984-2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(mat exceptions
  (begin
    (define ($$capture thunk)
      (with-output-to-string
        (lambda ()
          (call/cc
            (lambda (k)
              (with-exception-handler
                (lambda (x) (printf "default handler: ~s\n" x) (k))
                (lambda () (printf "~s\n" (thunk)))))))))
    (define-syntax $capture
      (syntax-rules ()
        [(_ e1 e2 ...) ($$capture (lambda () e1 e2 ...))]))
    #t)
  (equal?
    ($capture 'hello)
    "hello\n")
  (begin
    (define ($ex-test1) (raise 'oops) (printf "finished\n"))
    (define ($ex-test2) (printf "handler returned: ~s\n" (raise-continuable 'oops)) 'done)
    #t)
  (equal?
    ($capture (list ($ex-test1)))
    "default handler: oops\n")
  (equal?
    ($capture
      (list
        (with-exception-handler
          (lambda (arg) (printf "hello: ~s\n" arg))
          $ex-test1)))
    "hello: oops\ndefault handler: #<condition &non-continuable>\n")
  (equal?
    ($capture
      (list
        (with-exception-handler
          (lambda (arg) (raise (list arg)))
          $ex-test1)))
    "default handler: (oops)\n")
  (equal?
    ($capture (list ($ex-test2)))
    "default handler: oops\n")
  (equal?
    ($capture
      (list
        (with-exception-handler
          (lambda (arg) (printf "hello: ~s\n" arg) 17)
          $ex-test2)))
    "hello: oops\nhandler returned: 17\n(done)\n")
  (equal?
    ($capture
      (list
        (with-exception-handler
          (lambda (arg) (raise (list arg)))
          $ex-test2)))
    "default handler: (oops)\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (raise '()))))
    "(empty)\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (raise '(a . b)))))
    "((a . b))\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (raise 'oops))))
    "default handler: oops\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise '()))))))
    "just passing through...\n(empty)\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise '(a . b)))))))
    "just passing through...\n((a . b))\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise 'oops))))))
    "just passing through...\ndefault handler: oops\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo]
                    [else (raise 'hair)])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise '(a . b)))))))
    "just passing through...\n((a . b))\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo]
                    [else (raise 'hair)])
          (with-exception-handler
            (lambda (x) (printf "just passing through...\n") (raise x))
            (lambda () (raise 'oops))))))
    "just passing through...\ndefault handler: hair\n")
  (equal?
    ($capture
      (list
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (arg) (printf "outer handler: ~s\n" arg) (k 'fini))
              (lambda ()
                (guard (foo [(begin (printf "checking null\n") (null? foo)) 'empty]
                            [(begin (printf "checking pair\n") (pair? foo)) foo])
                  (dynamic-wind
                    (lambda () (printf "in\n"))
                    (lambda () (raise 'oops))
                    (lambda () (printf "out\n"))))))))))
    "in\nout\nchecking null\nchecking pair\nin\nouter handler: oops\nout\n(fini)\n")
  (equal?
    ($capture
      (list
        (guard (foo [(null? foo) 'empty]
                    [(pair? foo) foo])
          (with-exception-handler
            (lambda (x) (printf "returning...\n"))
            (lambda () (raise-continuable 'oops) 'continuing)))))
    "returning...\n(continuing)\n")
  (equal?
    ($capture
     ; test to make sure guard reraises with raise-continuable per r6rs errata
      (list
        (with-exception-handler
          (lambda (x) (printf "returning...\n"))
          (lambda ()
            (guard (foo [(null? foo) 'empty]
                        [(pair? foo) foo])
              (raise-continuable 'oops)
              'continuing)))))
    "returning...\n(continuing)\n")

  ;; check that a thread starts with the default exception-handler stack
  (or (not (threaded?))
      (let ([thread-exn-handled? #f])
        (let ([th
               (with-exception-handler
                (lambda (e) (set! thread-exn-handled? #t) (exit))
                (lambda ()
                  (parameterize ([base-exception-handler (lambda (exn) (exit))])
                    (fork-thread (lambda () (error 't ""))))))])
          (thread-join th)
          (not thread-exn-handled?))))

)

(mat assert
  (equal?
    (begin (assert #t) "yes")
    "yes")
  (equal?
    (assert (memq 'a '(1 2 a 3 4)))
    '(a 3 4))
  (error? ; assertion failed
    (assert (memq 'b '(1 2 a 3 4))))
  (equal?
    (begin (assert (< 3 4)) "yes")
    "yes")
  (equal?
    (guard (c [#t "yes"])
      (begin (assert #f) "no"))
    "yes")
  (equal?
    (guard (c [#t "yes"])
      (begin (assert (< 4 3)) "no"))
    "yes")
 ; make sure pattern variables and ellipses on RHS don't screw us up
  (equal?
    (guard (c [#t "oops"])
      (let-syntax ([q (lambda (x) #t)])
        (assert (q ...))
        "okay"))
    "okay")
  (equal?
    (guard (c [#t "oops"])
      (let-syntax ([q (lambda (x) #f)])
        (assert (q ...))
        "okay"))
    "oops")
  (error? ; assertion failed
    (let-syntax ([q (lambda (x) #f)])
      (assert (q ...))
      "okay"))
  (equal?
    (syntax-case '(a b c) ()
      [(x ...)
       (begin
         (assert (andmap symbol? #'(x ...)))
         #'((x . x) ...))])
    '((a . a) (b . b) (c . c)))
  (error? ; assertion failed
    (syntax-case '(a b 3) ()
      [(x ...)
       (begin
         (assert (andmap symbol? #'(x ...)))
         #'((x . x) ...))]))
)

(mat exceptions-r6rs ; r6rs examples
  (equal?
    ($capture
      (guard (con
               ((error? con)
                (if (message-condition? con)
                    (display (condition-message con))
                    (display "an error has occurred"))
                'error)
               ((violation? con)
                (if (message-condition? con)
                    (display (condition-message con))
                    (display "the program has a bug"))
                'violation))
        (raise
          (condition
            (make-error)
            (make-message-condition "I am an error")))))
    "I am an errorerror\n")
  (equal?
    ($capture
      (guard (con
               ((error? con)
                (if (message-condition? con)
                    (display (condition-message con))
                    (display "an error has occurred"))
                'error))
        (raise
          (condition
            (make-violation)
            (make-message-condition "I am an error")))))
    "default handler: #<compound condition>\n")
  (equal?
    ($capture
      (with-exception-handler
        (lambda (con)
          (cond
            ((not (warning? con))
             (raise con))
            ((message-condition? con)
             (display (condition-message con)))
            (else
             (display "a warning has been issued")))
          42)
        (lambda ()
          (+ (raise-continuable
               (condition
                 (make-warning)
                 (make-message-condition
                   "should be a number")))
             23))))
    "should be a number65\n")
)

(mat srfi-248
  (eqv? 1
    (with-unwind-handler
      (lambda (obj k) obj)
      (lambda () (raise 1))))
  (eqv? 2
    (with-unwind-handler
      (lambda (obj k) obj)
      (lambda () (raise-continuable 2))))
  (eqv? 3
    (with-unwind-handler
      (lambda (obj k) (+ 1 obj))
      (lambda ()
        (with-unwind-handler
          (lambda (obj k) (raise (+ 1 obj)))
          (lambda () (raise 1))))))
  (eqv? 4
    (with-unwind-handler
      (lambda (obj k)
        (and (non-continuable-violation? obj) 4))
      (lambda ()
        (with-unwind-handler
          (lambda (obj k) (k 2))
          (lambda () (raise 1))))))
  (eqv? 5
    (with-unwind-handler
      (lambda (obj k) (+ 2 (k (+ 3 obj))))
      (lambda () (+ 1 (raise-continuable -1)))))
  (eqv? 6
    (with-unwind-handler
      (lambda (obj k) (+ 2 (k (k (+ 3 obj)))))
      (lambda () (+ 1 (raise-continuable -1)))))
  (eqv? 7
    (guard (c [(number? c) (+ 1 c)])
      (raise 6)))
  (eqv? 8
    (guard (c [(symbol? c) #f]
              [else (+ 2 c)])
      (raise 6)))
  (eqv? 9
    (guard (c [else c])
      (guard (c [(symbol? c) #f])
        (raise 9))))
  (error? (with-unwind-handler #f void))
  (error? (with-unwind-handler values #f))
  (error?
    (with-unwind-handler
      (lambda (obj) obj)
      (lambda () (raise-continuable 'wrong-handler-arity))))
  (with-unwind-handler
    (lambda (obj k) (empty-continuation? k))
    (lambda () (raise-continuable 'tail-position)))
  (not
    (with-unwind-handler
      (lambda (obj k) (empty-continuation? k))
      (lambda () (not (raise-continuable 'non-tail-position)))))
  (begin
    (define ($248-tail-raise obj) (raise-continuable obj))
    #t)
  (with-unwind-handler
    (lambda (obj k) (empty-continuation? k))
    (lambda () ($248-tail-raise 'tail-call)))
  (guard (obj k [else (empty-continuation? k)])
    (raise-continuable 'guard-tail))
  (not
    (guard (obj k [else (empty-continuation? k)])
      (not (raise-continuable 'guard-non-tail))))
  (call/cc
    (lambda (escape)
      (with-unwind-handler
        (lambda (obj k) (escape (not (empty-continuation? k))))
        (lambda () (raise 'non-continuable)))))
  (equal? '(handled 21 41)
    (let ([saved #f])
      (let* ([first
              (with-unwind-handler
                (lambda (obj k) (set! saved k) 'handled)
                (lambda () (+ 1 (* 2 (raise-continuable 'save)))))]
             [second (saved 10)]
             [third (saved 20)])
        (list first second third))))
  (equal? '(10 20)
    (call-with-values
      (lambda ()
        (with-unwind-handler
          (lambda (obj k) (k 10 20))
          (lambda () (raise-continuable 'values))))
      list))
  (equal? '()
    (call-with-values
      (lambda ()
        (with-unwind-handler
          (lambda (obj k) (k))
          (lambda () (raise-continuable 'zero-values))))
      list))
  (equal? '(13 (exit enter exit enter exit enter))
    (let ([events '()])
      (let ([result
             (with-unwind-handler
               (lambda (obj k) (+ 3 (k (k 10))))
               (lambda ()
                 (dynamic-wind
                   (lambda () (set! events (cons 'enter events)))
                   (lambda () (raise-continuable 'wind))
                   (lambda () (set! events (cons 'exit events))))))])
        (list result events))))
  (begin
    (define ($248-call-with-state proc init)
      (define-condition-type &get-248 &condition
        make-get-248 get-248?)
      (define-condition-type &put-248 &condition
        make-put-248 put-248?
        (value put-248-value))
      (define (get) (raise-continuable (make-get-248)))
      (define (put val) (raise-continuable (make-put-248 val)))
      ((guard (obj k
                   [(get-248? obj)
                    (lambda (s) ((k s) s))]
                   [(put-248? obj)
                    (lambda (s) ((k) (put-248-value obj)))])
         (call-with-values (lambda () (proc get put))
           (lambda arg*
             (lambda (s) (apply values arg*)))))
       init))
    #t)
  (eqv? 10
    ($248-call-with-state (lambda (get put) (get)) 10))
  (eqv? 11
    ($248-call-with-state (lambda (get put) (+ 1 (get))) 10))
  (eqv? 11
    ($248-call-with-state
      (lambda (get put)
        (put (+ 1 (get)))
        (+ 1 (get)))
      9))
  (eqv? 12
    ($248-call-with-state
      (lambda (get put)
        (call/cc
          (lambda (c)
            (put 13)
            (c)))
        (get))
      12))
  (eqv? 12
    ($248-call-with-state
      (lambda (get put)
        (+ 2 (call/cc (lambda (k) (k (get))))))
      10))
  (eqv? 12
    ($248-call-with-state
      (lambda (get put)
        (+ 2 (call/1cc (lambda (k) (k (get))))))
      10))
  (equal? '(20 30)
    (with-exception-handler
      (lambda (obj) (values 20 30))
      (lambda ()
        (guard (obj k [(symbol? obj) 'unreachable])
          (call-with-values
            (lambda () (raise-continuable 10))
            list)))))
  (eqv? 12
    (guard (obj k
                [(and (number? obj) obj) => (lambda (n) (k (+ n 1)))])
      (+ 10 (raise-continuable 1))))
  (eqv? 103
    (+ 100
      (with-unwind-handler
        (lambda (obj k) (k 1))
        (lambda () (+ 2 (raise-continuable 'delimited))))))
  (equal? '(3 (second first))
    (let ([events '()])
      (let ([result
             (with-unwind-handler
               (lambda (obj k)
                 (set! events (cons obj events))
                 (k (if (eq? obj 'first) 1 2)))
               (lambda ()
                 (let* ([a (raise-continuable 'first)]
                        [b (raise-continuable 'second)])
                   (+ a b))))])
        (list result events))))
  (equal?
    '(11
      (outer-enter inner-enter inner-exit handler
        inner-enter inner-exit outer-exit))
    (let ([events '()])
      (define (record! event) (set! events (cons event events)))
      (let ([result
             (dynamic-wind
               (lambda () (record! 'outer-enter))
               (lambda ()
                 (with-unwind-handler
                   (lambda (obj k)
                     (record! 'handler)
                     (k 10))
                   (lambda ()
                     (dynamic-wind
                       (lambda () (record! 'inner-enter))
                       (lambda () (+ 1 (raise-continuable 'extent)))
                       (lambda () (record! 'inner-exit))))))
               (lambda () (record! 'outer-exit)))])
        (list result (reverse events)))))
  (equal? '((1) ())
    (with-continuation-mark 'outer 1
      (let ([result
             (with-unwind-handler
               (lambda (obj k)
                 (list
                   (continuation-marks->list
                     (current-continuation-marks) 'outer)
                   (continuation-marks->list
                     (current-continuation-marks) 'inner)))
               (lambda ()
                 (with-continuation-mark 'inner 2
                   (raise-continuable 'handler-marks))))])
        result)))
  (equal? '(#t 42)
    (call-with-values
      (lambda ()
        (with-unwind-handler
          (lambda (obj k)
            (call/cc
              (lambda (c)
                (values
                  (continuation-marks?
                    (continuation-next-marks c))
                  (k 42)))))
          (lambda () (raise-continuable 'call/cc))))
      list))
  (eqv? 17
    (with-unwind-handler
      (lambda (obj k)
        (call/1cc (lambda (c) (c (k 17)))))
      (lambda () (raise-continuable 'call/1cc))))
  ;; Detaching must not rewrite dynamic state shared with a native
  ;; continuation captured inside the protected thunk.
  (equal? '((O) (O))
    (let ([saved #f] [saved-marks #f])
      (call/cc
        (lambda (done)
          (with-continuation-mark '$248-shared-outer 'O
            (let ()
              (with-unwind-handler
                (lambda (obj k) 'handled)
                (lambda ()
                  (dynamic-wind
                    void
                    (lambda ()
                      (let ([v
                             (call/cc
                               (lambda (c)
                                 (set! saved c)
                                 'first))])
                        (if (eq? v 'again)
                            (done
                              (list
                                saved-marks
                                (continuation-marks->list
                                  (current-continuation-marks)
                                  '$248-shared-outer)))
                            (raise-continuable 'detach-shared))))
                    void)))
              (set! saved-marks
                (continuation-marks->list
                  (continuation-next-marks saved)
                  '$248-shared-outer))
              (saved 'again)))))))
  (equal? '(handled 21 41)
    (let ([saved #f])
      (let ([first
             (with-unwind-handler
               (lambda (obj k) (set! saved k) 'handled)
               (lambda () (+ 1 (* 2 (raise-continuable 'collect)))))])
        (collect)
        (list first (saved 10) (saved 20)))))
  (let ([saved #f] [weak #f])
    ((lambda (outer)
       (set! weak (weak-cons outer #f))
       (let ([result
              (with-unwind-handler
                (lambda (obj k) (set! saved k) 'handled)
                (lambda () (raise-continuable 'trim)))])
         (vector-set! outer 0 result)))
     (vector #f))
    (collect (collect-maximum-generation))
    (and (procedure? saved) (bwp-object? (car weak))))
  (let ([saved #f] [weak #f] [winder-weak #f])
    ((lambda (outside winder-state)
       (set! weak (weak-cons outside #f))
       (set! winder-weak (weak-cons winder-state #f))
       (with-continuation-mark 'srfi-248-outside outside
         (dynamic-wind
           (lambda () (vector-ref winder-state 0))
           (lambda ()
             (with-unwind-handler
               (lambda (obj k) (set! saved k) 'handled)
               (lambda () (+ 1 (raise-continuable 'detach-state)))))
           (lambda () (vector-ref winder-state 0)))))
     (vector 'outside)
     (vector 'winder-state))
    (collect (collect-maximum-generation))
    (and (procedure? saved)
         (bwp-object? (car weak))
         (bwp-object? (car winder-weak))
         (eqv? (saved 10) 11)
         (eqv? (saved 20) 21)))
  ;; A saved continuation can be composed and can raise again concurrently.
  (or (not (threaded?))
      (let ([saved #f])
        (let ([initial
               (with-unwind-handler
                 (lambda (obj k)
                   (cond
                     [(eq? obj 'concurrent-initial)
                      (set! saved k)
                      'captured]
                     [(and (pair? obj)
                           (eq? (car obj) 'concurrent-resumed))
                      (cdr obj)]
                     [else (raise obj)]))
                 (lambda ()
                   (let ([v (raise-continuable 'concurrent-initial)])
                     (raise-continuable
                       (cons 'concurrent-resumed (+ v 1000))))))])
          (let ([thread-count 8]
                [iteration-count 64])
            (let ([results (make-vector thread-count #f)])
              (let ([threads
                     (let loop ([i 0] [threads '()])
                       (if (= i thread-count)
                           threads
                           (loop (+ i 1)
                             (cons
                               (fork-thread
                                 (lambda ()
                                   (vector-set! results i
                                     (let loop ([j 0])
                                       (or (= j iteration-count)
                                           (let ([v
                                                  (+ i
                                                    (* j thread-count))])
                                             (and
                                               (= (saved v) (+ v 1000))
                                               (loop (+ j 1)))))))))
                               threads))))])
                (for-each thread-join threads)
                (and (eq? initial 'captured)
                     (andmap values (vector->list results)))))))))
  (let ([saved #f] [weak #f])
    (with-unwind-handler
      (lambda (obj k) (set! saved k) 'handled)
      (lambda () (+ 1 (raise-continuable 'composition-target))))
    ((lambda (caller)
       (set! weak (weak-cons caller #f))
       (saved 10)
       (vector-set! caller 0 'done))
     (vector #f))
    (collect (collect-maximum-generation))
    (and (procedure? saved) (bwp-object? (car weak))))
  (equal? '(handled 65536)
    (let ([saved #f])
      (let ([first
             (with-unwind-handler
               (lambda (obj k) (set! saved k) 'handled)
               (lambda () (+ 1 (* 2 (raise-continuable 'stress)))))])
        (let loop ([i 0] [sum 0])
          (if (= i 256)
              (list first sum)
              (begin
                (when (zero? (modulo i 32)) (collect))
                (loop (+ i 1) (+ sum (saved i)))))))))
  (eqv? 28
    (let ([saved #f])
      (with-unwind-handler
        (lambda (obj k) (set! saved k) 'handled)
        (lambda ()
          (let loop ([n 32])
            (if (zero? n)
                (raise-continuable 'winder-stress)
                (dynamic-wind void
                  (lambda () (loop (- n 1)))
                  void)))))
      (let loop ([i 0] [sum 0])
        (if (= i 8)
            sum
            (begin
              (collect 0 1)
              (loop (+ i 1) (+ sum (saved i))))))))
  (begin
    (define ($248-make-marked-winder-k depth)
      (let ([saved #f])
        (with-unwind-handler
          (lambda (obj k) (set! saved k) 'handled)
          (lambda ()
            (let loop ([i depth])
              (if (zero? i)
                  (raise-continuable 'marked-winder-stress)
                  (with-continuation-mark i i
                    (dynamic-wind void
                      (lambda () (loop (- i 1)))
                      void))))))
        saved))
    #t)
  (let ([k32 ($248-make-marked-winder-k 32)]
        [k64 ($248-make-marked-winder-k 64)])
    (define (resume-bytes k)
      ;; Exclude one-time allocation of the lazy composition-target table.
      (k 0)
      (collect (collect-maximum-generation))
      (let ([before (statistics)])
        (k 0)
        (sstats-bytes (sstats-difference (statistics) before))))
    (let ([b32 (resume-bytes k32)]
          [b64 (resume-bytes k64)])
      (and (> b32 0) (< b64 (* 4 b32)))))
  (with-unwind-handler
    (lambda (obj k) (empty-continuation? k))
    (lambda ()
      (with-continuation-mark 'srfi-248-tail 'saved
        (raise-continuable 'marked-tail))))
  (equal? '(7 saved)
    (with-unwind-handler
      (lambda (obj k) (k 7))
      (lambda ()
        (with-continuation-mark 'srfi-248-mark 'saved
          (list
            (raise-continuable 'marked)
            (continuation-marks-first
              (current-continuation-marks)
              'srfi-248-mark))))))
  (begin
    (define-condition-type $248-&yield &condition
      $248-make-yield $248-yield?
      (value $248-yield-value))
    (define ($248-make-coroutine-generator proc)
      (define (yield val)
        (raise-continuable ($248-make-yield val)))
      (define thunk
        (lambda ()
          (guard (c k
                    [($248-yield? c)
                     (set! thunk k)
                     ($248-yield-value c)])
            (proc yield)
            (eof-object))))
      (lambda () (thunk)))
    (define ($248-generator->list g)
      (let loop ()
        (let ([x (g)])
          (if (eof-object? x)
              '()
              (cons x (loop))))))
    (define ($248-for-each->fold for-each)
      (lambda (proc nil)
        ((guard (c k
                  [($248-yield? c)
                   (lambda (s)
                     ((k) (proc s ($248-yield-value c))))])
           (for-each
             (lambda (x)
               (raise-continuable ($248-make-yield x))))
           values)
         nil)))
    (define ($248-xcons x y) (cons y x))
    #t)
  (equal? '(1 2)
    (let ([g
           ($248-make-coroutine-generator
             (lambda (yield)
               (yield 1)
               (yield 2)))])
      ($248-generator->list g)))
  (equal? '(3 2 1)
    (($248-for-each->fold
       (lambda (f)
         (for-each
           (lambda (x)
             (call-with-values (lambda () (f x))
               (lambda ignored (void))))
           '(1 2 3))))
     $248-xcons '()))
  (begin
    (define-condition-type $248-&control &condition
      $248-make-control $248-control?
      (control $248-control-procedure))
    (define-record-type ($248-prompt $248-make-prompt $248-prompt?)
      (fields
        (immutable first $248-prompt-first)
        (immutable second $248-prompt-second)))
    (define-syntax $248-first
      (syntax-rules ()
        [(_ e)
         (call-with-values (lambda () e)
           (case-lambda
             [(p)
              (if ($248-prompt? p)
                  (($248-prompt-first p))
                  p)]
             [val* (apply values val*)]))]))
    (define-syntax $248-second
      (syntax-rules ()
        [(_ e)
         (call-with-values (lambda () e)
           (case-lambda
             [(p)
              (if ($248-prompt? p)
                  (($248-prompt-second p))
                  p)]
             [val* (apply values val*)]))]))
    (define-syntax $248-prompt0
      (syntax-rules ()
        [(_ e)
         ($248-second
           (guard (c k
                     [($248-control? c)
                      (if (empty-continuation? k)
                          ($248-make-prompt
                            (lambda () (raise-continuable c))
                            (lambda ()
                              (($248-control-procedure c) values)))
                          (let ([k
                                 (lambda arg*
                                   ($248-first (apply k arg*)))])
                            ($248-make-prompt
                              (lambda ()
                                (call-with-values
                                  (lambda () (raise-continuable c))
                                  k))
                              (lambda ()
                                (($248-control-procedure c) k)))))])
             e))]))
    (define-syntax $248-control0
      (syntax-rules ()
        [(_ k e)
         (raise-continuable
           ($248-make-control (lambda (k) e)))]))
    #t)
  (eqv? 1 ($248-prompt0 1))
  (eqv? 2 ($248-prompt0 (+ 1 ($248-control0 k 2))))
  (eqv? 4 ($248-prompt0 (+ 1 ($248-control0 k (k (k 2))))))
  (eqv? 0
    ($248-prompt0
      (+ 4
        ($248-prompt0
          (+ 1
            (let ([x ($248-control0 k (k 0))])
              ($248-control0 l x)))))))
  (procedure?
    (eval 'with-unwind-handler (environment '(srfi :248))))
  (procedure?
    (eval 'empty-continuation?
      (environment '(srfi :248 exceptions))))
  (eqv? 248
    (eval '(guard (obj k [else obj]) (raise 248))
      (environment '(rnrs) '(srfi :248))))
  (error? (empty-continuation? values))
)

(mat srfi-248-racket-control
  ;; These cases exercise the generic continuation behaviors covered by
  ;; Racket's prompt and control tests through the SRFI-248 interface.
  (begin
    (define-condition-type $248-&racket-capture &condition
      $248-make-racket-capture $248-racket-capture?)
    (define ($248-racket-prompt thunk)
      (with-unwind-handler
        (lambda (obj k)
          (if ($248-racket-capture? obj)
              k
              (raise obj)))
        thunk))
    (define ($248-racket-capture)
      ((raise-continuable ($248-make-racket-capture))))
    (define ($248-racket-non-tail x) (values x))
    (define-condition-type $248-&racket-shift &condition
      $248-make-racket-shift $248-racket-shift?
      (procedure $248-racket-shift-procedure))
    (define-syntax $248-racket-reset0
      (syntax-rules ()
        [(_ e)
         (guard (obj k
                   [($248-racket-shift? obj)
                    (($248-racket-shift-procedure obj) k)])
           e)]))
    (define-syntax $248-racket-shift0
      (syntax-rules ()
        [(_ k e)
         (raise-continuable
           ($248-make-racket-shift (lambda (k) e)))]))
    #t)
  (eqv? 10 ($248-racket-prompt (lambda () 10)))
  (equal? '(10 11)
    (call-with-values
      (lambda () ($248-racket-prompt (lambda () (values 10 11))))
      list))
  (equal? '()
    (call-with-values
      (lambda () ($248-racket-prompt (lambda () (values))))
      list))
  (equal? '(captured 2048 2049)
    (let ([saved #f])
      (let* ([first
              (with-unwind-handler
                (lambda (obj k) (set! saved k) 'captured)
                (lambda ()
                  (let loop ([n 2048])
                    (if (zero? n)
                        (raise-continuable 'deep)
                        (+ 1 (loop (- n 1)))))))]
             [second (saved 0)]
             [third (saved 1)])
        (list first second third))))
  (let ([k ($248-racket-prompt $248-racket-capture)])
    (and
      (eqv? 12 (k (lambda () 12)))
      (eqv? 13
        (k (lambda ()
             (k (lambda ()
                  (k (lambda () 13)))))))
      (equal? '(12 13)
        (call-with-values
          (lambda () (k (lambda () (values 12 13))))
          list))))
  (equal? '(1000 -1000 1001 -1001 1002 -1002)
    (let ([k1 ($248-racket-prompt $248-racket-capture)]
          [k2 ($248-racket-prompt $248-racket-capture)])
      (let ([k3
             ($248-racket-prompt
               (lambda ()
                 (k1 (lambda () ($248-racket-capture)))))])
        (list
          (k1 (lambda () (k2 (lambda () 1000))))
          (k1 (lambda () (k2 (lambda () -1000))))
          (k3 (lambda () 1001))
          (k3 (lambda () -1001))
          (k1 (lambda () (k3 (lambda () 1002))))
          (k1 (lambda () (k3 (lambda () -1002))))))))
  (eqv? 8
    (let ([k ($248-racket-prompt $248-racket-capture)])
      (call/cc
        (lambda (escape)
          (+ 99 (k (lambda () (escape 8))))))))
  (call/cc
    (lambda (done)
      (let ([native #f] [phase 0])
        (let ([k0 ($248-racket-prompt $248-racket-capture)])
          (let ([result
                 (k0
                   (lambda ()
                     (let ([v
                            (call/cc
                              (lambda (c)
                                (set! native c)
                                'first))])
                       (if (eq? v 'again)
                           ($248-racket-capture)
                           'normal))))])
            (if (zero? phase)
                (begin
                  (set! phase 1)
                  (native 'again))
                (done
                  (and (procedure? result)
                       (eqv? 42
                         (result (lambda () 42)))))))))))
  (equal? '(73 (post2 post pre pre2))
    (let ([events '()])
      (define (record! event) (set! events (cons event events)))
      (let ([k
             ($248-racket-prompt
               (lambda ()
                 (dynamic-wind
                   (lambda () (record! 'pre))
                   $248-racket-capture
                   (lambda () (record! 'post)))))])
        (set! events '())
        (let ([result
               (dynamic-wind
                 (lambda () (record! 'pre2))
                 (lambda () (k (lambda () 73)))
                 (lambda () (record! 'post2)))])
          (list result events)))))
  (equal? '((71) (81))
    (let ([k
           ($248-racket-prompt
             (lambda ()
               (with-continuation-mark 'x 71
                 ($248-racket-capture))))])
      (with-continuation-mark 'y 81
        ($248-racket-non-tail
          (k (lambda ()
               (list
                 (continuation-marks->list
                   (current-continuation-marks) 'x)
                 (continuation-marks->list
                   (current-continuation-marks) 'y))))))))
  (equal? '(2 1)
    (let ([k
           ($248-racket-prompt
             (lambda ()
               (with-continuation-mark 'key 2
                 ($248-racket-capture))))])
      (with-continuation-mark 'key 1
        ($248-racket-non-tail
          (k (lambda ()
               (continuation-marks->list
                 (current-continuation-marks) 'key)))))))
  (equal? '(enter-1 enter-2 work exit-2 exit-1)
    (let ([events '()])
      (define (record! event) (set! events (cons event events)))
      (define (capture-winder enter exit)
        ($248-racket-prompt
          (lambda ()
            (dynamic-wind
              (lambda () (record! enter))
              $248-racket-capture
              (lambda () (record! exit))))))
      (let ([k1 (capture-winder 'enter-1 'exit-1)]
            [k2 (capture-winder 'enter-2 'exit-2)])
        (set! events '())
        (k1 (lambda ()
              (k2 (lambda () (record! 'work)))))
        (reverse events))))
  (equal? '(inner-enter body inner-exit outer-exit)
    (let ([events '()])
      (define (record! event) (set! events (cons event events)))
      (dynamic-wind
        (lambda () (record! 'outer-enter))
        (lambda ()
          (let ([k
                 ($248-racket-prompt
                   (lambda ()
                     (dynamic-wind
                       (lambda () (record! 'inner-enter))
                       $248-racket-capture
                       (lambda () (record! 'inner-exit)))))])
            (set! events '())
            (k (lambda () (record! 'body)))))
        (lambda () (record! 'outer-exit)))
      (reverse events)))
  (equal? '(1)
    (with-continuation-mark 'shared 1
      ($248-racket-non-tail
        (let ([k ($248-racket-prompt $248-racket-capture)])
          (k (lambda ()
               (continuation-marks->list
                 (current-continuation-marks) 'shared)))))))
  (equal? '(captured caller)
    (let ([captured (make-parameter 'base)]
          [caller (make-parameter 'base)])
      (let ([k
             ($248-racket-prompt
               (lambda ()
                 (parameterize ([captured 'captured])
                   ($248-racket-capture))))])
        (parameterize ([caller 'caller])
          ($248-racket-non-tail
            (k (lambda () (list (captured) (caller)))))))))
  (equal?
    '((enter (1) (2)) (body (1) (2)) (exit (1) (2)))
    (let ([events '()])
      (define (marks key)
        (continuation-marks->list
          (current-continuation-marks) key))
      (define (record! event)
        (set! events
          (cons (list event (marks 'captured) (marks 'caller))
            events)))
      (let ([k
             ($248-racket-prompt
               (lambda ()
                 (with-continuation-mark 'captured 1
                   (dynamic-wind
                     (lambda () (record! 'enter))
                     $248-racket-capture
                     (lambda () (record! 'exit))))))])
        (set! events '())
        (with-continuation-mark 'caller 2
          ($248-racket-non-tail
            (k (lambda () (record! 'body)))))
        (reverse events))))
  (equal?
    '((enter (1) (2)) (body (1) (2)) (exit (1) (2)))
    (let ([events '()])
      (define (marks key)
        (continuation-marks->list
          (current-continuation-marks) key))
      (define (record! event)
        (set! events
          (cons (list event (marks 'captured) (marks 'caller))
            events)))
      (let ([k
             ($248-racket-prompt
               (lambda ()
                 (with-continuation-mark 'captured 1
                   (dynamic-wind
                     (lambda () (record! 'enter))
                     $248-racket-capture
                     (lambda () (record! 'exit))))))])
        (set! events '())
        (call/cc
          (lambda (escape)
            (with-continuation-mark 'caller 2
              ($248-racket-non-tail
                (k (lambda ()
                     (record! 'body)
                     (escape 'done)))))))
        (reverse events))))
  (equal? '(enter body exit)
    (let ([events '()])
      (define (record! event) (set! events (cons event events)))
      (let ([k
             ($248-racket-prompt
               (lambda ()
                 (dynamic-wind #t
                   (lambda () (record! 'enter))
                   $248-racket-capture
                   (lambda () (record! 'exit)))))])
        (set! events '())
        (k (lambda () (record! 'body)))
        (reverse events))))
  (eqv? 117
    (+ 10
      ($248-racket-reset0
        (+ 2
          ($248-racket-shift0 k
            (+ 100 (k (k 3))))))))
  (equal? '()
    ($248-racket-reset0
      (cons 'a
        ($248-racket-reset0
          ($248-racket-shift0 f
            ($248-racket-shift0 g '()))))))
  (eqv? 17
    (let ([k ($248-racket-prompt $248-racket-capture)])
      (let loop ([n 256] [thunk (lambda () 17)])
        (if (zero? n)
            (thunk)
            (let ([next thunk])
              (loop (- n 1) (lambda () (k next))))))))
  (guard (obj [else (condition? obj)])
    (let ([k ($248-racket-prompt $248-racket-capture)])
      (k (lambda () (error 'srfi-248 "composition")))))
)

(mat racket-control
  (begin
    (define $control-environment
      (apply environment
        (append
          (list
            '(rnrs)
            '(only (chezscheme)
               collect
               create-exception-state
               current-exception-state
               with-unwind-handler))
          (if (threaded?)
              (list '(only (chezscheme) fork-thread thread-join))
              '())
          (list '(chezscheme control)))))
    (define ($control-eval expression)
      (eval expression $control-environment))
    #t)
  (eqv? 42
    ($control-eval
      '(reset (+ 1 (shift k (k 41))))))
  (eqv? 13
    ($control-eval
      '(reset (+ 1 (shift k (+ (k 5) (k 6)))))))
  (eqv? 5
    ($control-eval
      '(reset (+ 1 (shift k (k (shift k2 5)))))))
  (eqv? 5
    ($control-eval
      '(reset
         (guard (condition [else 'caught])
           (shift k 5)))))
  (eqv? 5
    ($control-eval
      '(reset
         (with-unwind-handler
           (lambda (condition continuation) 'caught)
           (lambda () (shift k 5))))))
  (eqv? 105
    ($control-eval
      '(reset
         (+ 100
           (reset
             (+ 1 (shift k (shift k2 5))))))))
  (eqv? 5
    ($control-eval
      '(reset0
         (+ 100
           (reset0
             (+ 1 (shift0 k (shift0 k2 5))))))))
  (eqv? 105
    ($control-eval
      '(reset
         (+ 100
           (reset
             (+ 1 (shift0 k (shift0 k2 5))))))))
  (eqv? 105
    ($control-eval
      '(reset0
         (+ 100
           (reset0
             (+ 1 (shift k (shift k2 5))))))))
  (eqv? 17
    ($control-eval
      '(let ([tag (make-continuation-prompt-tag 'tag)])
         (reset-at tag
           (+ 10 (shift-at tag k (k 7)))))))
  (equal? '(0 11 caught)
    ($control-eval
      '(let ([saved #f])
         (list
           (reset
             (+ 10
               (shift/1 k
                 (set! saved k)
                 0)))
           (saved 1)
           (guard (condition [else 'caught])
             (saved 2))))))
  (equal? '(1 2 3)
    ($control-eval
      '(reset
         (call-with-values
           (lambda () (shift/1 k (k 1 2 3)))
           list))))
  ;; Promotion before the first invocation uses the copying fallback while
  ;; retaining the public one-shot contract.
  (equal? '(0 11 caught)
    ($control-eval
      '(let ([saved #f])
         (let ([first
                (reset
                  (+ 10
                    (shift/1 k
                      (set! saved k)
                      0)))])
           (collect)
           (list
             first
             (saved 1)
             (guard (condition [else 'caught])
               (saved 2)))))))
  (equal? '(enter exit body enter exit)
    ($control-eval
      '(let ([events '()])
         (define (record! event)
           (set! events (cons event events)))
         (reset
           (dynamic-wind
             (lambda () (record! 'enter))
             (lambda ()
               (shift/1 k
                 (k (begin (record! 'body) 0))))
             (lambda () (record! 'exit))))
         (reverse events))))
  (eqv? 17
    ($control-eval
      '(let ([tag (make-continuation-prompt-tag 'tag)])
         (reset-at tag
           (+ 10 (shift/1-at tag k (k 7)))))))
  (eqv? 105
    ($control-eval
      '(reset
         (+ 100
           (reset
             (+ 1 (shift0/1 k (shift0/1 k2 5))))))))
  (eqv? 5
    ($control-eval
      '(let ([tag (make-continuation-prompt-tag 'tag)])
         (reset0-at tag
           (+ 1 (shift0/1-at tag k (k 4)))))))
  (eqv? 11
    ($control-eval
      '(let ([outer (make-continuation-prompt-tag 'outer)]
             [inner (make-continuation-prompt-tag 'inner)])
         (reset-at outer
           (+ 100
             (reset-at inner
               (+ 10
                 (shift-at outer k 11))))))))
  (equal? '(1 2)
    ($control-eval
      '(reset
         (call-with-values
           (lambda () (shift k (k 1 2)))
           list))))
  (equal? '(0 11 12)
    ($control-eval
      '(let ([saved #f])
         (let ([first
                (reset
                  (+ 10
                    (shift k
                      (set! saved k)
                      0)))])
           (list first (saved 1) (saved 2))))))
  ;; Composing a continuation onto a dynamic context must not reinterpret an
  ;; ordinary empty list in the captured stack as a winder list.
  (equal? '(7 6 5 4 3 2 1 0)
    ($control-eval
      '(let ([saved #f])
         (let ([first
                (reset
                  (let loop ([i 0] [values '()])
                    (if (= i 8)
                        values
                        (let ([value
                               (shift k
                                 (set! saved k)
                                 'suspended)])
                          (loop (+ i 1) (cons value values))))))])
           (let loop ([i 0] [result first])
             (if (= i 8)
                 result
                 (loop (+ i 1) (saved i))))))))
  (equal?
    '(enter exit body enter exit)
    ($control-eval
      '(let ([events '()])
         (define (record! event)
           (set! events (cons event events)))
         (reset
           (dynamic-wind
             (lambda () (record! 'enter))
             (lambda ()
               (shift k
                 (k (begin (record! 'body) 0))))
             (lambda () (record! 'exit))))
         (reverse events))))
  ($control-eval
    '(let ([tag (make-continuation-prompt-tag)])
       (and
         (continuation-prompt-tag? tag)
         (continuation-prompt-tag?
           (default-continuation-prompt-tag))
         (eq? (default-continuation-prompt-tag)
              (default-continuation-prompt-tag))
         (not (eq? tag (make-continuation-prompt-tag))))))
  (equal? "#<continuation-prompt-tag:tag>"
    ($control-eval
      '(call-with-values
         open-string-output-port
         (lambda (port get-output-string)
           (write (make-continuation-prompt-tag 'tag) port)
           (get-output-string)))))
  (eqv? 5
    ($control-eval
      '(let ([original (current-exception-state)]
             [replacement (create-exception-state)])
         (dynamic-wind
           (lambda () #f)
           (lambda ()
             (reset
               (current-exception-state replacement)
               (shift k 5)))
           (lambda ()
             (current-exception-state original))))))
  (eq? 'caught
    ($control-eval
      '(let ([original (current-exception-state)]
             [saved #f]
             [tag (make-continuation-prompt-tag 'saved)])
         (dynamic-wind
           (lambda () #f)
           (lambda ()
             (reset-at tag
               (set! saved (current-exception-state)))
             (current-exception-state saved)
             (guard (condition [else 'caught])
               (shift-at tag k 'unexpected)))
           (lambda ()
             (current-exception-state original))))))
  (or (not (threaded?))
      (eqv? 101
        ($control-eval
          '(let ([result #f])
             (thread-join
               (fork-thread
                 (lambda ()
                   (set! result
                     (+ 100 (shift k (k 1)))))))
             result))))
  (error?
    ($control-eval '(make-continuation-prompt-tag "tag")))
  (error?
    ($control-eval '(reset-at 'not-a-tag 1)))
  (error?
    ($control-eval
      '(shift-at (make-continuation-prompt-tag 'missing) k 1)))
  (procedure?
    (eval 'reset (environment '(chezscheme))))
)

(mat conditions-r6rs ; r6rs examples
  (begin
    (define-record-type ($co-&cond1 $co-make-cond1 $co-real-cond1?)
      (parent &condition)
      (fields (immutable x $co-real-cond1-x)))
    (define $co-cond1?
      (condition-predicate
        (record-type-descriptor $co-&cond1)))
    (define $co-cond1-x
      (condition-accessor
        (record-type-descriptor $co-&cond1)
        $co-real-cond1-x))
    (define $co-foo ($co-make-cond1 'foo))
    #t)
  (condition? $co-foo)
  ($co-cond1? $co-foo)
  (eq? ($co-cond1-x $co-foo) 'foo)
  (begin
    (define-record-type ($co-&cond2 $co-make-cond2 $co-real-cond2?)
      (parent &condition)
      (fields
       (immutable y $co-real-cond2-y)))
    (define $co-cond2?
      (condition-predicate
        (record-type-descriptor $co-&cond2)))
    (define $co-cond2-y
      (condition-accessor
         (record-type-descriptor $co-&cond2)
         $co-real-cond2-y))
    (define $co-bar ($co-make-cond2 'bar))
  #t)
  (condition? (condition $co-foo $co-bar))
  ($co-cond1? (condition $co-foo $co-bar))
  ($co-cond2? (condition $co-foo $co-bar))
  ($co-cond1? (condition $co-foo))
  (list?
    (memq
      ($co-real-cond1? (condition $co-foo))
      '(#t #f)))
  (not ($co-real-cond1? (condition $co-foo $co-bar)))
  (eq? ($co-cond1-x (condition $co-foo $co-bar)) 'foo)
  (eq? ($co-cond2-y (condition $co-foo $co-bar)) 'bar)
  (equal?
    (simple-conditions (condition $co-foo $co-bar))
    (list $co-foo $co-bar))
  (equal?
    (simple-conditions (condition $co-foo (condition $co-bar)))
    (list $co-foo $co-bar))
  (begin
    (define-condition-type $co-&c &condition $co-make-c $co-c? (x $co-c-x))
    (define-condition-type $co-&c1 $co-&c $co-make-c1 $co-c1? (a $co-c1-a))
    (define-condition-type $co-&c2 $co-&c $co-make-c2 $co-c2? (b $co-c2-b))
    (define $co-v1 ($co-make-c1 "V1" "a1"))
    #t)
  ($co-c? $co-v1)
  ($co-c1? $co-v1)
  (not ($co-c2? $co-v1))
  (equal? ($co-c-x $co-v1) "V1")
  (equal? ($co-c1-a $co-v1) "a1")
  (begin
    (define $co-v2 ($co-make-c2 "V2" "b2"))
    (define $co-v3 (condition ($co-make-c1 "V3/1" "a3") ($co-make-c2 "V3/2" "b3")))
    (define $co-v4 (condition $co-v1 $co-v2))
    (define $co-v5 (condition $co-v2 $co-v3))
    #t)
  ($co-c? $co-v2)
  (not ($co-c1? $co-v2))
  ($co-c2? $co-v2)
  (equal? ($co-c-x $co-v2) "V2")
  (equal? ($co-c2-b $co-v2) "b2")
  ($co-c? $co-v3)
  ($co-c1? $co-v3)
  ($co-c2? $co-v3)
  (equal? ($co-c-x $co-v3) "V3/1")
  (equal? ($co-c1-a $co-v3) "a3")
  (equal? ($co-c2-b $co-v3) "b3")
  ($co-c? $co-v4)
  ($co-c1? $co-v4)
  ($co-c2? $co-v4)
  (equal? ($co-c-x $co-v4) "V1")
  (equal? ($co-c1-a $co-v4) "a1")
  (equal? ($co-c2-b $co-v4) "b2")
  ($co-c? $co-v5)
  ($co-c1? $co-v5)
  ($co-c2? $co-v5)
  (equal? ($co-c-x $co-v5) "V2")
  (equal? ($co-c1-a $co-v5) "a3")
  (equal? ($co-c2-b $co-v5) "b2")
)

(mat system-exceptions
  (equal?
    ($capture
     ; from r6rs
      (guard (con
               ((error? con)
                (display "error opening file")
                #f))
        (call-with-input-file "/probably/not/here" read)))
    "error opening file#f\n")
  (guard (c [else (and (assertion-violation? c)
                       (not (implementation-restriction-violation? c)))])
    (let ()
      (define-record-type foo (fields x))
      (foo-x 17)))
)

(mat exception-state
  (#%$record? (current-exception-state))
  (not (record? (current-exception-state)))
  (eq?
    (call/cc
      (lambda (k)
        (parameterize ([current-exception-state
                        (create-exception-state
                          (lambda (x)
                            (if (eq? x 'oops)
                                (raise 'rats)
                                (k x))))])
          (raise 'oops))))
    'rats)
)
