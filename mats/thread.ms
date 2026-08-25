;;; thread.ms
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

; why can't this come after misc.ms?

(define-syntax when-threaded
  (lambda (x)
    (syntax-case x ()
      [(_ e ...)
       (if (threaded?)
           #'(begin (void) e ...)
           #'(void))])))

(mat engine-thread
  (let ()
    (define fattercode
      '(module (fatterfib fatter slimmer zero)
        (define zero? null?)
        (define 1+ list)
        (define 1- car)
        (define zero '())
        (define (fatter n)
          (if (= n 0)
              zero
              (1+ (fatter (- n 1)))))
        (define (slimmer x)
          (if (zero? x)
              0
              (+ (slimmer (1- x)) 1)))
        (define fatter+
          (lambda (x y)
            (if (zero? y)
                x
                (fatter+ (1+ x) (1- y)))))
        (define fatterfib
          (lambda (x)
            (if (or (zero? x) (zero? (1- x)))
                (1+ zero)
                (fatter+ (fatterfib (1- x)) (fatterfib (1- (1- x)))))))))
    (define fatter-eval
      (lambda (eval loc)
        (lambda ()
          (let f ([n 10] [a 0])
            (if (= n 0)
                (set-car! loc a)
                (f (- n 1)
                   (+ a (eval `(let ()
                                 ,fattercode
                                 (slimmer (fatterfib (fatter 10))))))))))))
  
    (define (engine-eval-test n)
      (define thread-list '())
      (define fork-thread
        (lambda (x)
          (set! thread-list (cons (make-engine x) thread-list))))
      (define locs (map list (make-list n 0)))
      (for-each
        (lambda (x) (fork-thread (fatter-eval interpret x)))
        locs)
      (let f ([q thread-list])
        (unless (null? q)
          ((car q)
            10
            (lambda (t . r) (f (cdr q)))
            (lambda (x) (f (append (cdr q) (list x)))))))
      (map car locs))

    (equal? (engine-eval-test 7) '(890 890 890 890 890 890 890)))
 )

(when-threaded
(define thread-foreign-library-loaded? #f)

(define (load-thread-foreign-library!)
  (unless thread-foreign-library-loaded?
    (load-shared-object
      (let ([p (find-source "foreign1.so")])
        (if (or (path-absolute? p)
                (not (equal? "" (path-parent p))))
            p
            (path-build "." p))))
    (set! thread-foreign-library-loaded? #t)))

(mat thread
  (let ([m (make-mutex)] [c (make-condition)]
        [m2 (make-mutex 'mname)] [c2 (make-condition 'cname)])
    (and (mutex? m)
         (thread-condition? c)
         (mutex? m2)
         (thread-condition? c2)
         (not (mutex? c))
         (not (thread-condition? m))
         (not (mutex? c2))
         (not (thread-condition? m2))
         (not (mutex-name m))
         (not (condition-name c))
         (eq? 'mname (mutex-name m2))
         (eq? 'cname (condition-name c2))
         (not (mutex? 'mutex))
         (not (thread-condition? 'condition))))
  (thread? (get-initial-thread))
  (begin
    (include "thread-check.ss")
    (define $time-in-range?
      (lambda (start stop target)
        (let ([t (time-difference stop start)])
          (<= (abs (- (+ (time-second t) (* (time-nanosecond t) 1e-9)) target))
              0.2))))
    (define-syntax $retry-for-timing
      ;; timing tests can go wrong, especially on a share machine like
      ;; one for CI, so try a few times to reduce the chance of
      ;; failure due to a real-time delay
      (lambda (stx)
        (syntax-case stx ()
          [(_ e) #'(let loop ([n 5])
                     (or e
                         (and (> n 0)
                              (loop (- n 1)))))])))
    (andmap procedure? (list $threads $fib $thread-check $time-in-range?)))
  ($thread-check)
  (not (= (let ([n #f])
            (fork-thread (lambda () (set! n (get-thread-id))))
            (let f () (if n n (begin ($yield) (f)))))
          0))
  ($thread-check)
  (= (let ([n #f])
       (fork-thread (lambda () (set! n (get-process-id))))
       (let f () (if n n (begin ($yield) (f)))))
     (get-process-id))
  ($thread-check)
  (let ()
    (define fat+
      (lambda (x y)
        (if (zero? y)
            x
            (fat+ (1+ x) (1- y)))))
    (define fatfib
      (lambda (x)
        (if (< x 2)
            1
            (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
    (define-syntax parallel-list
      (syntax-rules ()
        [(_ x ...)
         (let ([v (make-vector (length '(x ...)) #f)]
               [m (make-mutex)]
               [c (make-condition)]
               [n (length '(x ...))])
           (and
            (with-mutex m
              (map (lambda (p i) (p i))
                (list (lambda (i)
                        (fork-thread
                         (lambda ()
                           (vector-set! v i x)
                           (with-mutex m
                             (set! n (- n 1))
                             (when (= n 0)
                               (condition-signal c))))))
                  ...)
                (iota (length '(x ...))))
              (condition-wait c m (make-time 'time-duration 0 60)))
            (vector->list v)))]))
    (define result
      (parallel-list (fatfib 26) (fatfib 27) (fatfib 28)
        (fatfib 29) (fatfib 30) (fatfib 31)))
    (or (equal? result '(196418 317811 514229 832040 1346269 2178309))
        (errorf #f "result=~s" result)))
  ($thread-check)
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (let* ([start (current-time)]
              [r (condition-wait c m (make-time 'time-duration 250000000 1))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 1.25))))))
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (let* ([start (current-time)]
              [r (condition-wait c m
                   (add-duration start (make-time 'time-duration 250000000 1)))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 1.25))))))
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (let* ([start (current-time)]
              [r (condition-wait c m (make-time 'time-duration 0 -1))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 0.0))))))
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (let* ([start (current-time)]
              [r (condition-wait c m
                   (add-duration start (make-time 'time-duration 0 -1)))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 0.0))))))
  ($retry-for-timing
   (let ([m (make-mutex)] [c (make-condition)])
     (with-mutex m
       (fork-thread
        (lambda ()
          (with-mutex m (sleep (make-time 'time-duration 250000000 0)))))
       (let* ([start (current-time)]
              [r (condition-wait c m (make-time 'time-duration 100000000 0))]
              [stop (current-time)])
         (and (not r)
              ($time-in-range? start stop 0.25))))))
  (let ([count 300] [live 0] [live-m (make-mutex)])
    (parameterize ([collect-request-handler
                    (lambda ()
                      (set! count (- count 1))
                      (collect))])
      (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
      (define (gc-test n)
        (set! live n)
        (do ([i 0 (+ i 1)])
            ((= i n))
          (fork-thread
            (lambda ()
              (chew 0)
              (with-mutex live-m (set! live (- live 1)))))))
      (gc-test 4)
      (chew 0))
     ; wait for the others to die
    (let f () (unless (= live 0) ($yield) (f)))
    #t)
  ($thread-check)
  (let ([count 300] [live 0] [live-m (make-mutex)])
    (parameterize ([collect-request-handler
                    (lambda ()
                      (set! count (- count 1))
                      (collect))])
      (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
      (define (gc-test n)
        (set! live n)
        (do ([i 0 (+ i 1)])
            ((= i n))
          (fork-thread
            (lambda ()
              (chew 0)
              (with-mutex live-m (set! live (- live 1)))))))
      (let ([m1 (make-mutex)]
            [m2 (make-mutex)]
            [c1 (make-condition)]
            [c2 (make-condition)])
       ; suspend one thread on a condition, waiting for it to get there
        (with-mutex m1
          (fork-thread
            (lambda ()
              (with-mutex m1
                (condition-signal c1)
                (condition-wait c1 m1)
                (condition-signal c1))))
          (condition-wait c1 m1))
       ; try to get another suspended on a mutex
        (mutex-acquire m2)
        (fork-thread
          (lambda ()
            (with-mutex m2 (condition-signal c2))))
       ; start some threads a-allocating 
        (gc-test 3)
       ; join in the fun
        (chew 0)
       ; release the suspended threads
        (with-mutex m1
          (condition-signal c1)
          (condition-wait c1 m1))
        (condition-wait c2 m2)
        (mutex-release m2)))
   ; wait for the others to die
    (let f () (unless (= live 0) ($yield) (f)))
    #t)
  ($thread-check)
  (let ([count 300] [live 0] [live-m (make-mutex)])
    (define fat+
      (lambda (x y)
        (if (zero? y)
            x
            (fat+ (1+ x) (1- y)))))
    (define fatfib
      (lambda (x)
        (if (< x 2)
            1
            (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
    (parameterize ([collect-request-handler
                    (lambda ()
                      (set! count (- count 1))
                      (collect))])
      (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
      (define (gc-test n)
        (set! live n)
        (do ([i 0 (+ i 1)])
            ((= i n))
          (fork-thread
            (lambda ()
              (chew 0)
              (with-mutex live-m (set! live (- live 1)))))))
      (let ([m1 (make-mutex)]
            [m2 (make-mutex)]
            [c1 (make-condition)]
            [c2 (make-condition)])
       ; suspend one thread on a condition, waiting for it to get there
        (with-mutex m1
          (fork-thread
            (lambda ()
              (with-mutex m1
                (condition-signal c1)
                (condition-wait c1 m1)
                (condition-signal c1))))
          (condition-wait c1 m1))
       ; try to get another suspended on a mutex
        (mutex-acquire m2)
        (fork-thread
          (lambda ()
            (with-mutex m2 (condition-signal c2))))
       ; start some threads a-allocating 
        (gc-test 5)
       ; create threads that die quickly while waiting for
       ; our count collections to occur
        (let ([live 0] [live-m (make-mutex)])
          (let f ()
            (when (> count 0)
              (fatfib 20)
              (with-mutex live-m
               ; cap the number of quickly dying threads, or ti3nb
               ; runs out of memory somewhere later in the mat run.
               ; theory is that virtual address space gets chopped
               ; up and that the main thread can't access virtual
               ; memory previously assigned to a child thread.
                (if (< live 20)
                   ; don't panic if we try to create too many...
                    (guard (c [#t (values)])
                      (fork-thread
                        (lambda ()
                          (with-mutex live-m (set! live (- live 1)))))
                      (set! live (+ live 1)))))
              (f))))
       ; release the suspended threads
        (with-mutex m1
          (condition-signal c1)
          (condition-wait c1 m1))
        (condition-wait c2 m2)
        (mutex-release m2)))
   ; wait for the others to die
    (let f () (unless (= live 0) ($yield) (f)))
    #t)
  ($thread-check)
  (let ([count 30])
    (parameterize ([collect-request-handler
                    (lambda ()
                      (set! count (- count 1))
                      (collect))])
      (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
      (define (gc-test4 n)
        (define fib
          (lambda (x)
            (if (< x 2)
                1
                (+ (fib (- x 1)) (fib (- x 2))))))
        ; stall collection for a bit
        (disable-interrupts)
        (do ([i 0 (+ i 1)])
            ((= i n))
          (fork-thread (lambda () (enable-interrupts) (chew 0))))
        (fib 35)
        (enable-interrupts))
      (gc-test4 4)
      (chew 0)
      #t))
  ($thread-check)
  (eqv?
    (let ()
      (define m (make-mutex))
      (define fib
        (lambda (n)
          (if (with-mutex m (< n 2))
              1
              (+ (fib (- n 1)) (fib (- n 2))))))
      (fib 20))
    10946)
  ($thread-check)
  (let ()
    (define m (make-mutex))
    (define mgrab
      (lambda (n)
        (unless (fxzero? n)
          (mutex-acquire m)
          (mutex-release m)
          (mgrab (fx- n 1)))))
    (mgrab 100)
    #t)
  ($thread-check)
  (let ()
    (define m (make-mutex))
    (define recmgrab
      (lambda (n)
        (set! m (make-mutex))
        (let f ([n n])
          (unless (fxzero? n)
            (mutex-acquire m)
            (f (fx- n 1))
            (mutex-release m)))
        (fork-thread (lambda () (mutex-acquire m) #;(pretty-print 'bye)))))
    (recmgrab 100)
    #t)
  ($thread-check)
  (eqv?
    (let ([cnt 0] [ans 0])
      (define m (make-mutex))
      (mutex-acquire m)
      (fork-thread
        (rec f
          (lambda ()
            (with-mutex m (set! cnt (+ cnt 1)))
            (if (= ans 0)
                (begin ($yield) (f))
                (set! ans (- ans))))))
      (mutex-release m)
      (let f () (when (= cnt 0) (begin ($yield) (f))))
      (set! ans 17)
      (let f () (if (< ans 0) ans (begin ($yield) (f)))))
    -17)
  ($thread-check)
  (eqv?
    (let ()
      (define ans 0)
      (define cnt 0)
      (define m (make-mutex))
      (define c (make-condition))
      (define (f n)
        (fork-thread
          (lambda ()
            (with-mutex m
              (set! ans (+ ans n))
              (set! cnt (+ cnt 1))
              (condition-wait c m)
              (set! cnt (- cnt 1))))))
      (f 1)
      (f 2)
      (f 4)
      (let f () (unless (= cnt 3) (begin ($yield) (f))))
      (with-mutex m (condition-broadcast c))
      (let f () (unless (= cnt 0) (begin ($yield) (f))))
      ans)
    7)
  ($thread-check)
  (let ([nthreads (length ($threads))])
    (define-record bounded-queue (i)
      ([vec (make-vector i)]
       [mutex (make-mutex)]
       [ready (make-condition)]
       [room (make-condition)]
       [waiting 0]
       [die? #f]))
    
    (define queue-empty?
      (lambda (bq)
        (with-mutex (bounded-queue-mutex bq)
          (= (bounded-queue-i bq)
             (vector-length (bounded-queue-vec bq))))))
    
    (define enqueue!
      (lambda (item bq)
        (let ([mutex (bounded-queue-mutex bq)])
          (with-mutex mutex
            (let loop ()
              (unless (bounded-queue-die? bq)
                (if (zero? (bounded-queue-i bq))
                    (begin
                      (condition-wait (bounded-queue-room bq) mutex)
                     ; we grab the mutex when we wake up, but some other thread may
                     ; beat us to the punch
                      (loop))
                    (let ([i (- (bounded-queue-i bq) 1)])
                      (vector-set! (bounded-queue-vec bq) i item)
                      (set-bounded-queue-i! bq i)
                      (unless (zero? (bounded-queue-waiting bq))
                        (condition-signal (bounded-queue-ready bq)))))))))))
    
    (define dequeue!
      (lambda (bq)
        (let ([mutex (bounded-queue-mutex bq)])
          (with-mutex mutex
            (let loop ()
              (unless (bounded-queue-die? bq)
                (if (= (bounded-queue-i bq) (vector-length (bounded-queue-vec bq)))
                    (begin
                      (set-bounded-queue-waiting! bq
                        (+ (bounded-queue-waiting bq) 1))
                      (condition-wait (bounded-queue-ready bq) mutex)
                      (set-bounded-queue-waiting! bq
                        (- (bounded-queue-waiting bq) 1))
                     ; we grab the mutex when we wake up, but some other thread may
                     ; beat us to the punch
                      (loop))
                    (let ([i (bounded-queue-i bq)])
                      (let ([item (vector-ref (bounded-queue-vec bq) i)])
                        (set-bounded-queue-i! bq (+ i 1))
                        (condition-signal (bounded-queue-room bq))
                        item)))))))))
    
    (define job-queue)
    
    (define fib
      (lambda (n)
        (if (< n 2)
            n
            (+ (fib (- n 2)) (fib (- n 1))))))

    (define make-job
      (let ([count 0])
        (lambda (n)
          (set! count (+ count 1))
          (printf "Adding job #~s = (lambda () (fib ~s))\n" count n)
          (cons count (lambda () (fib n))))))
    
    (define make-producer
      (lambda (n)
        (rec producer
          (lambda ()
            (printf "producer ~s posting a job\n" n)
            (enqueue! (make-job (+ 20 (random 10))) job-queue)
            (if (bounded-queue-die? job-queue)
                (printf "producer ~s dying\n" n)
                (producer))))))
    
    (define make-consumer
      (lambda (n)
        (rec consumer
          (lambda ()
            (printf "consumer ~s looking for a job~%" n)
            (let ([job (dequeue! job-queue)])
              (if (bounded-queue-die? job-queue)
                  (printf "consumer ~s dying\n" n)
                  (begin
                    (printf "consumer ~s executing job #~s~%" n (car job))
                    (printf "consumer ~s computed:  ~s~%" n ((cdr job)))
                    (consumer))))))))
    
    (define (bq-test np nc)
      (set! job-queue (make-bounded-queue (max nc np)))
      (do ([np np (- np 1)])
          ((<= np 0))
          (fork-thread (make-producer np)))
      (do ([nc nc (- nc 1)])
          ((<= nc 0))
          (fork-thread (make-consumer nc))))

    (bq-test 3 4)
    (set! ans (fib 35))
   ; flush out the waiting producers and consumers
    (set-bounded-queue-die?! job-queue #t)
    (let ()
      (let f ()
        (unless (= (length ($threads)) nthreads)
          (with-mutex (bounded-queue-mutex job-queue)
            (condition-signal (bounded-queue-room job-queue))
            (condition-signal (bounded-queue-ready job-queue)))
          ($yield)
          (f))))
    (eqv? ans 9227465))
  ($thread-check)
  (let ([ans #f])
    (define fattercode
      '(module (fatterfib fatter slimmer zero)
        (define zero? null?)
        (define 1+ list)
        (define 1- car)
        (define zero '())
        (define (fatter n)
          (if (= n 0)
              zero
              (1+ (fatter (- n 1)))))
        (define (slimmer x)
          (if (zero? x)
              0
              (+ (slimmer (1- x)) 1)))
        (define fatter+
          (lambda (x y)
            (if (zero? y)
                x
                (fatter+ (1+ x) (1- y)))))
        (define fatterfib
          (lambda (x)
            (if (or (zero? x) (zero? (1- x)))
                (1+ zero)
                (fatter+ (fatterfib (1- x)) (fatterfib (1- (1- x)))))))))
    (define fatter-eval
      (lambda (eval loc)
        (lambda ()
          (let f ([n 10] [a 0])
            (if (= n 0)
                (set-car! loc a)
                (f (- n 1)
                   (+ a (eval `(let ()
                                 ,fattercode
                                 (slimmer (fatterfib (fatter 10))))))))))))
  
    (define (thread-eval-test n)
      (define locs (map list (make-list n 0)))
      (for-each
       ; compiler is not reentrant wrt threads, so must use interpret
        (lambda (loc) (fork-thread (fatter-eval interpret loc)))
        locs)
      (let f ()
        (when (ormap zero? (map car locs))
          ($yield)
          (f)))
      (map car locs))

    (equal? (thread-eval-test 7) '(890 890 890 890 890 890 890)))
  ($thread-check)
  (equal?
    (let ([x1 #f] [x2 #f] [x3 #f] [x4 #f] [x5 #f])
      (define p (make-thread-parameter 0))
      (set! x1 (p))
      (p 3)
      (set! x2 (p))
      (fork-thread
        (lambda ()
          (set! x3 (p))
          (p 5)
          (set! x4 (p))))
      (let f () (unless x4 ($yield) (f)))
      (set! x5 (p))
      (list x1 x2 x3 x4 x5))
    '(0 3 3 5 3))
  ($thread-check)
  (equal?
    (let ([x1 #f] [x2 #f] [x3 #f] [x4 #f] [x5 #f])
      (define p (make-thread-parameter 0))
      (define m (make-mutex))
      (define c (make-condition))
      (set! x1 (p))
      (p 3)
      (set! x2 (p))
      (with-mutex m
        (fork-thread
          (lambda ()
            (set! x3 (p))
            (p 5)
            (set! x4 (p))
            (with-mutex m (condition-signal c))))
        (condition-wait c m))
      (set! x5 (p))
      (list x1 x2 x3 x4 x5))
    '(0 3 3 5 3))
  ($thread-check)
  (let ()
    (define done? #f)
    (define m (make-mutex))
    (define spin
      (lambda ()
        (unless done?
          (if (mutex-acquire m #f)
              (set! done? #t)
              (begin ($yield) (spin))))))
    (fork-thread spin)
    (fork-thread spin)
    (spin)
    done?)
  ($thread-check)
  (eqv?
    (let ()
      (define-record accumulator ()
        ([acc 0]
         [increments 0]
         [(immutable mutex) (make-mutex)]))
      (define incr!
        (lambda (a n)
          (with-mutex (accumulator-mutex a)
            (set-accumulator-acc! a (+ (accumulator-acc a) n))
            (set-accumulator-increments! a (+ (accumulator-increments a) 1)))))
      (define fact
        (lambda (x)
          (if (= x 0)
              1
              (* x (fact (- x 1))))))
      (define foo (lambda (x) (string-length (number->string (fact x)))))
      (let ()
        (define acc (make-accumulator))
        (define bar
          (lambda (x)
            (fork-thread
              (lambda ()
                (incr! acc (foo x))))))
        (bar 1100)
        (bar 1200)
        (bar 1300)
        (bar 1400)
        (bar 1500)
        (bar 1400)
        (bar 1300)
        (bar 1200)
        (bar 1100)
        (let f ()
          (unless (= (accumulator-increments acc) 9)
            ($yield)
            (f)))
        (accumulator-acc acc)))
    30777)
  ($thread-check)
  (let ()
    (define fat+
      (lambda (x y)
        (if (zero? y)
            x
            (fat+ (1+ x) (1- y)))))
    (define fatfib
      (lambda (x)
        (if (< x 2)
            1
            (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
    (set! ans (fatfib 30))
    (collect)
    (eqv? ans 1346269))
  ($thread-check)
  (eq?
    (let ()
      (define bt (foreign-procedure "(cs)backdoor_thread" (scheme-object) iptr))
      (let f ([n 100])
        (let ([q 0])
          (unless (= n 0)
            (let ([p (lambda ()
                       (let ([count 10])
                         (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
                         (parameterize ([collect-request-handler
                                         (lambda ()
                                           (set! count (- count 1))
                                           (collect))])
                           (chew 0)))
                       (set! q (+ q 7)))])
              (lock-object p)
              (bt p)
              (let f () (when (= q 0) ($yield) (f)))
              (let f () (unless (= (length ($threads)) 1) ($yield) (f)))
              (unlock-object p))
            (unless (= q 14) (errorf #f "~s isn't 14" q))
            (f (- n 1)))))
      'cool)
    'cool)
  ($thread-check)
  ;; copy of the test above, this time trying an immobile box instead of locked procedure
  (eq?
    (let ()
      (define bt (foreign-procedure "(cs)backdoor_thread" (scheme-object) iptr))
      (let f ([n 100])
        (let ([q 0])
          (unless (= n 0)
            (let ([p (lambda ()
                       (let ([count 10])
                         (define chew (lambda (x) (when (> count 0) (chew (list 0)))))
                         (parameterize ([collect-request-handler
                                         (lambda ()
                                           (set! count (- count 1))
                                           (collect))])
                           (chew 0)))
                       (set! q (+ q 7)))])
              (let ([b (box-immobile p)])
                (bt b)
                (let f () (when (= q 0) ($yield) (f)))
                (let f () (unless (= (length ($threads)) 1) ($yield) (f)))
                (keep-live b)))
            (unless (= q 14) (errorf #f "~s isn't 14" q))
            (f (- n 1)))))
      'cool)
    'cool)
  ($thread-check)
  (let ()
    (define m (make-mutex))
    (define c (make-condition))
    (define nthreads 4)
    (define saved-condition #f)
    (with-mutex m
      (let f ([n nthreads])
        (unless (= n 0)
          (fork-thread
            (lambda ()
              (guard (c [else (set! saved-condition c)])
                (let g ([k 1000])
                  (unless (= k 0)
                    (let ([op (open-file-output-port (format "testfile~s.ss" n) (file-options replace))])
                      (port-file-compressed! op)
                      (put-u8 op 104)
                      (close-port op))
                    (g (- k 1)))))
              (with-mutex m
                (set! nthreads (- nthreads 1))
                (when (= nthreads 0)
                  (condition-signal c)))))
          (f (- n 1))))
      (condition-wait c m))
    (when saved-condition (raise saved-condition))
    #t)
  ($thread-check)
  (let ()
    (define m (make-mutex))
    (define c (make-condition))
    (define nthreads 4)
    (define saved-condition #f)
    (with-mutex m
      (let f ([n nthreads])
        (unless (= n 0)
          (fork-thread
            (lambda ()
              (guard (c [else (set! saved-condition c)])
                (let g ([k 1000])
                  (unless (= k 0)
                    (let ([ip (open-file-input-port (format "testfile~s.ss" n))])
                      (port-file-compressed! ip)
                      (let ([b (get-u8 ip)])
                        (unless (eqv? b 104)
                          (error #f "thread ~s read wrong value ~s" n b)))
                      (close-port ip))
                    (g (- k 1)))))
              (with-mutex m
                (set! nthreads (- nthreads 1))
                (when (= nthreads 0) (condition-signal c)))))
          (f (- n 1))))
      (condition-wait c m))
    (when saved-condition (raise saved-condition))
    #t)
  ($thread-check)
  (begin
    (module ($tt-count $tt-done!)
      (define n 0)
      (define done-mutex (make-mutex))
      (define $tt-count (lambda () n))
      (define ($tt-done!)
        (with-mutex done-mutex 
          (set! n (+ n 1)))))
    (define $tt-spam (make-thread-parameter 'main))
    (define $tt-run (make-thread-parameter void))
    (define $tt-global-mutex (make-mutex))
    (define $tt-fat
      (make-thread-parameter
        (let f ([n 100] [ls (oblist)])
          (define (pick-rem ls)
            (let f ([ls ls] [i (random (length ls))])
              (if (fx= i 0)
                  (values (car ls) (cdr ls))
                  (let-values ([(x d) (f (cdr ls) (fx- i 1))])
                    (values x (cons (car ls) d))))))
          (if (= n 0)
              '()
              (let-values ([(x ls) (pick-rem ls)])
                (cons x (f (- n 1) ls)))))))
   ; original test used copy-environment, which we don't use because
   ; environments are never collected and we don't want the remaining
   ; mats to take forever to run because the heap is full of lots of
   ; unreclaimable junk.  So we simulate some of the chores of copying
   ; an environment
    (define ($tt-chew ls)
      (let ([g* (map
                  (lambda (x)
                    (let ([g (gensym)])
                      (putprop g '*cte* (cons 'alias x))
                      (when (#%$top-level-bound? x)
                        (#%$set-top-level-value! g (#%$top-level-value x)))
                      g))
                  ls)])
        (map (lambda (g) (cdr (getprop g '*cte*))) g*)))
    (with-output-to-file "testfile.ss"
      (lambda ()
        (pretty-print
          '($tt-run (lambda ()
                      (do ([i 2 (1- i)])
                        ((= i 0))
                        (with-mutex $tt-global-mutex
                          (printf
                            "thread ~s iteration ~s\n"
                            ($tt-spam)
                            i))
                        (with-mutex $tt-global-mutex
                          ($tt-fat ($tt-chew ($tt-fat)))))))))
      'replace)
    (parameterize ([collect-request-handler
                    (lambda ()
                      (printf "~s collecting\n" ($tt-spam))
                      (collect))]
                   [collect-notify #t])
      (time
        (do ([i 15 (1- i)])
            ((= i 0))
          (printf "Pass ~a~%" i)
          (time
            (let* ([thread-count 25]
                   #;[compile-mutex (make-mutex)]
                   [thread-finished-mutex (make-mutex)]
                   [thread-finished-count 0]
                   [program-finished-condition (make-condition)]
                   [console-mutex $tt-global-mutex]
                   [saved-condition #f])
              (do ([i thread-count (1- i)])
                  ((= i 0))
                (mutex-acquire $tt-global-mutex)
                (parameterize ([$tt-fat ($tt-chew ($tt-fat))])
                  (mutex-release $tt-global-mutex)
                  (fork-thread
                    (lambda ()
                      (guard (c [else (set! saved-condition c)])
                        ($tt-spam i)
                        (with-mutex console-mutex
                          (printf "Starting thread ~s...~%" i))
                       ; mutex should no longer be needed as of v7.3
                        #;(with-mutex compile-mutex (load "testfile.ss"))
                       ; let's see if we can load from source w/o a mutex
                        (load "testfile.ss")
                        (($tt-run))
                       ; now let's see if we can compile and load object code
                       ; w/o a mutex
                        (compile-file "testfile.ss" (format "testfile.~s" i))
                        (load (format "testfile.~s" i))
                        (($tt-run))
                        (with-mutex console-mutex
                          (printf "Finished ~s.~%" i)))
                      ($tt-done!)
                      (with-mutex thread-finished-mutex
                        (set! thread-finished-count
                          (1+ thread-finished-count))
                        (when (= thread-finished-count
                                 thread-count)
                          (condition-signal
                            program-finished-condition)))))))
              (with-mutex thread-finished-mutex
                (unless (= thread-finished-count thread-count)
                  (condition-wait
                    program-finished-condition
                    thread-finished-mutex)))
              ($tt-done!)
              (when saved-condition (raise saved-condition)))))))
    (eqv? ($tt-count) 390))
  ($thread-check)
 ; following tests garbage collection of code created and run by multiple
 ; threads.  gc is done by one thread.  will the others find their instruction
 ; and data caches properly synchronized?
  (let ([thread-count 2] [iterations 10000])
    (equal?
      (parameterize ([collect-trip-bytes (expt 2 15)]
                     [collect-generation-radix 2]) ; using 1 risks extreme slowness via major collections
        (let ([out '()]
              [out-mutex (make-mutex)]
              [out-condition (make-condition)]
              [error? #f])
          (do ([i 0 (+ i 1)])
              ((= i thread-count))
            (fork-thread
              (lambda ()
                (guard (c [#t (with-mutex out-mutex (set! error? c))])
                  (let ([n (eval `(let ()
                                    (define zero? null?)
                                    (define 1+ list)
                                    (define 1- car)
                                    (define zero '())
                                    (define (fatter n)
                                      (if (= n 0)
                                          zero
                                          (1+ (fatter (- n 1)))))
                                    (define (slimmer x)
                                      (if (zero? x)
                                          0
                                          (+ (slimmer (1- x)) 1)))
                                    (define fatter+
                                      (lambda (x y)
                                        (if (zero? y)
                                            x
                                            (fatter+ (1+ x) (1- y)))))
                                    (define fatterfib
                                      (lambda (x)
                                        (if (or (zero? x) (zero? (1- x)))
                                            (1+ zero)
                                            (fatter+ (fatterfib (1- x)) (fatterfib (1- (1- x)))))))
                                    (let loop ([n ,iterations] [a 0])
                                      (if (= n 0)
                                          a
                                          (loop (- n 1) (+ a (slimmer (fatterfib (fatter 10)))))))))])
                    (with-mutex out-mutex
                      (set! out (cons n out))
                      (condition-signal out-condition)))))))
          (let f ()
            (printf "waiting for ~s more thread(s)\n" (- thread-count (length out)))
            (with-mutex out-mutex
              (unless (or error? 
                          (= (length out) thread-count)
                          (= (length ($threads)) $nthreads))
              (condition-wait out-condition out-mutex)))
            (cond
              [error? (raise error?)]
              [(or (= (length out) thread-count)
                   (= (length ($threads)) $nthreads))
               (printf "done\n")
               out]
              [else (f)]))))
      (make-list thread-count (* 89 iterations))))
  ($thread-check)
 ; following tries to verify proper synchronization when top-level exports
 ; build up system property list.  need a module with at least 21 exports to
 ; force syntax.ss to consult property list for imports
  (let ([thread-count 20]
        [iterations 100]
        [syms '(a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
                a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24)])
    (equal?
      (let ([out '()] [out-mutex (make-mutex)] [error? #f])
        (do ([i 0 (+ i 1)])
            ((= i thread-count))
          (fork-thread
            (lambda ()
              (guard (c [#t (set! error? c)])
                (do ([j 0 (+ j 1)])
                    ((= j iterations))
                  (eval `(letrec-syntax
                           ([frob 
                             (syntax-rules ()
                               [(_ m r) (frobh m r ,syms ,syms ())])]
                            [frobh
                             (syntax-rules ()
                               [(_ m r (s ...) () (q ...))
                                (module m (r q ...)
                                  (define-syntax r
                                    (identifier-syntax
                                      (list ,(+ (* i thread-count) j) q ...)))
                                  (define q 's)
                                  ...)]
                               [(_ m r (a ...) (p0 p ...) (q ...))
                                (frobh m r (a ...) (p ...) ($tx q ...))])])
                           (frob
                             ,(string->symbol (format "m~s-~s" i j))
                             $tx)))
                  (let ([v (eval `(let ()
                                    (import ,(string->symbol (format "m~s-~s" i j)))
                                    $tx))])
                    (with-mutex out-mutex
                      (set! out (cons v out)))))))))
        (let f ([n 0])
          (cond
            [error? (raise error?)]
            [(or (= (length out) (* thread-count iterations))
                 (= (length ($threads)) $nthreads))
             (sort (lambda (x y) (< (car x) (car y))) out)]
            [else
             (when (= (modulo n 100000) 0) (printf "waiting ~s ~s\n" n (length out)))
             (f (+ n 1))])))
      (sort (lambda (x y) (< (car x) (car y)))
        (let f ([i 0])
          (if (= i thread-count)
              '()
              (let g ([j 0])
                (if (= j iterations)
                    (f (+ i 1))
                    (cons (cons (+ (* i thread-count) j) syms) (g (+ j 1))))))))))
  ($thread-check)
  (let ([thread-count 20]
        [iterations 100])
    (equal?
      (let ([out '()] [out-mutex (make-mutex)] [error? #f])
        (do ([i 0 (+ i 1)])
            ((= i thread-count))
          (fork-thread
            (lambda ()
              (guard (c [#t (set! error? c)])
                (do ([j 0 (+ j 1)])
                    ((= j iterations))
                  (eval `(letrec-syntax
                           ([frob 
                             (syntax-rules ()
                               [(_ a)
                                (begin
                                  (define-syntax $ty (identifier-syntax ,(+ (* i thread-count) j)))
                                  (define-syntax a (identifier-syntax $ty)))])])
                           (frob ,(string->symbol (format "a~s-~s" i j)))))
                  (let ([v (eval (string->symbol (format "a~s-~s" i j)))])
                    (with-mutex out-mutex
                      (set! out (cons v out)))))))))
        (let f ([n 0])
          (cond
            [error? (raise error?)]
            [(or (= (length out) (* thread-count iterations))
                 (= (length ($threads)) $nthreads))
             (sort < out)]
            [else
             (when (= (modulo n 100000) 0) (printf "waiting ~s ~s\n" n (length out)))
             (f (+ n 1))])))
      (sort <
        (let f ([i 0])
          (if (= i thread-count)
              '()
              (let g ([j 0])
                (if (= j iterations)
                    (f (+ i 1))
                    (cons (+ (* i thread-count) j) (g (+ j 1))))))))))
  ($thread-check)
  ; this mat has some inherent starvation issues, with the main thread
  ; looping rather than waiting on a condition at initialization time and
  ; other threads looping rather than waiting on a condition when looking
  ; for work to steal.  these looping threads can hog the cpu without
  ; doing anything useful, causing progress to stall or halt.  this
  ; manifests as occasional indefinite delays under Windows and OpenBSD,
  ; and it has the potential to cause the same on other operating systems.
  ; it's not clear how to fix the mat without changing it fundamentally.
  #;(eqv?
    (let () ; from Ryan Newton
    
      (define-syntax ASSERT
        (lambda (x)
          (syntax-case x ()
            [(_ expr) #'(or expr (errorf 'ASSERT "failed: ~s" (IFCHEZ #'expr (format-syntax #'expr))))]
            ;; This form is (ASSERT integer? x) returning the value of x.
            [(_ fun val) #'(let ([v val])
                             (if (fun v) v                       
                                 (errorf 'ASSERT "failed: ~s\n Value which did not satisfy above predicate: ~s" 
                                        (IFCHEZ #'fun (format-syntax #'fun))
                                        v)))]
            )))
    
      (define test-depth 25) ;; Make a tree with 2^test-depth nodes.
    
      (define vector-build
        (lambda (n f)
          (let ([v (make-vector n)])
            (do ([i 0 (fx+ i 1)])
                ((= i n) v)
              (vector-set! v i (f i))
              ))))
      
      ;; STATE:
    
      ;; Each thread's stack has a list of frames, from newest to oldest.
      ;; We use a lock-free approach for mutating/reading the frame list.
      ;; Therefore, a thief might steal an old inactive frame, but this poses no problem.
      ;; 
      ;; A thread's "stack" must be as efficient as possible, because
      ;; it essentially replaces the native scheme stack where par calls
      ;; are concerned.  (I wonder if continuations can serve any purpose here.)
      ;; Note, head is the "bottom" and tail is the "top".  We add to tail.
      (define-record shadowstack (id head tail frames))
      
      ;; Frames are locked individually.
      ;; status may be 'available, 'stolen, or 'done
      (define-record shadowframe  (mut status oper argval))
    
      ;; There's also a global list of threads:
      (define allstacks '#()) ;; This is effectively immutable.
      (define par-finished #f)
      ;; And a mutex for global state:
      (define global-mut (make-mutex))
      (define threads-registered 1)
    
      ;; A new stack has no frames, but has a (hopefully) unique ID:
      (define (new-stack) 
        (make-shadowstack (random 10000) 
          0            ;; Head pointer.
          0            ;; Tail pointer.
          (vector-build 50 
            (lambda (_) (make-shadowframe (make-mutex) #f #f #f)))))
    
      ;; A per-thread parameter.
      (define this-stack (make-thread-parameter (new-stack)))
      
      ;; Mutated below:
      (define numprocessors #f)
    
      ;; We spin until everybody is awake.
      (define (wait-for-everybody)
        (let wait-for-threads ([n 0])
         ; attempt to prevent apparent occasional starvation on openbsd
          (when (= n 0) (printf "  ~s ~s\n" threads-registered numprocessors))
          (unless (= threads-registered numprocessors)
            (wait-for-threads (mod n 1000)))))
    
        ;; DEBUGGING:
      ;;  Pick a print:
      ;   (define (print . args) (with-mutex global-mut (apply printf args) (flush-output-port)))
      ;   (define (print . args) (apply printf args))
         (define (print . args) (void)) ;; fizzle
    
    
      ;; ----------------------------------------
    
      (define (init-par num-cpus)
        (printf "\n  Initializing PAR system for ~s threads.\n" num-cpus)
        (with-mutex global-mut   
          (ASSERT (eq? threads-registered 1))
          (set! numprocessors num-cpus)
          (set! allstacks (make-vector num-cpus))
          (vector-set! allstacks 0 (this-stack))
          ;; We fork N-1 threads (the original one counts)
          (do ([i 1 (fx+ i 1)]) ([= i num-cpus] (void))
            (vector-set! allstacks i (make-worker))))
        (wait-for-everybody)
        (printf "Everyone's awake!\n"))
    
      (define (shutdown-par) (set! par-finished #t))
      (define (par-status) 
        (printf "Par status:\n  par-finished ~s\n  allstacks: ~s\n  stacksizes: ~s\n\n"
                par-finished (vector-length allstacks)
                (map shadowstack-tail (vector->list allstacks))))
    
      ;; ----------------------------------------
    
      ;; Try to do work and mark it as done.
      (define (steal-work! frame)
        (and (eq? 'available (shadowframe-status frame))
             (mutex-acquire (shadowframe-mut frame) #f) ;; Don't block on it
             ;; From here on out we've got the mutex:
             (if (eq? 'available (shadowframe-status frame)) ;; If someone beat us here, we fizzle
                 #t 
                 (begin (mutex-release (shadowframe-mut frame)) 
                        #f))
             (begin 
               ;;(printf "STOLE work! ~s\n" frame)
               (set-shadowframe-status! frame 'stolen)
               (mutex-release (shadowframe-mut frame)) 
               ;; Then let go to do the real work:
               (set-shadowframe-argval! frame 
                  ((shadowframe-oper frame) (shadowframe-argval frame)))
               (set-shadowframe-status! frame 'done)           
               #t)))
    
      (define (find-and-steal-once!)
        (let* ([ind (random numprocessors)]
               [stack (vector-ref allstacks ind)])
          (let* ([frames (shadowstack-frames stack)]
                 [tl     (shadowstack-tail stack)])
            (let frmloop ([i 0])
              (if (fx= i tl) 
                  #f ;; No work on this processor, try again. 
                  (if (steal-work! (vector-ref frames i))
                      #t
                      (frmloop (fx+ 1 i))))))))
    
      (define (make-worker)
        (define stack (new-stack))
        (fork-thread (lambda ()                
                       (this-stack stack) ;; Initialize stack. 
                       (with-mutex global-mut ;; Register our existence.
                         (set! threads-registered (add1 threads-registered)))
                       ;; Steal work forever:
                       (let forever ()
                         (unless par-finished
                           (find-and-steal-once!)
                           (forever)))))
        stack)
    
      (define-syntax pcall
        (syntax-rules ()
          [(_ op (f x) e2)
           (let ([stack (this-stack)])
             (define (push! oper val)
               (let ([frame (vector-ref (shadowstack-frames stack) (shadowstack-tail stack))])
                 ;; Initialize the frame
                 (set-shadowframe-oper!   frame oper)
                 (set-shadowframe-argval! frame val)
                 (set-shadowframe-status! frame  'available)
                 (set-shadowstack-tail! stack (fx+ (shadowstack-tail stack) 1))
                 frame))
             (define (pop!) (set-shadowstack-tail! stack (fx- (shadowstack-tail stack) 1)))
    
             (let ([frame (push! f x)])
               (let ([val1 e2])
                 ;; We're the parent, when we get to this frame, we lock it off from all other comers.
                 ;; Thieves should do non-blocking probes.
                 (let waitloop ()
                   (mutex-acquire (shadowframe-mut frame))
                   (case (shadowframe-status frame)
                     [(available)             
                      (set-shadowframe-status! frame 'stolen) ;; Just in case...
                      (pop!) ;; Pop before we even start the thunk.
                      (mutex-release (shadowframe-mut frame))
                      (op ((shadowframe-oper frame) (shadowframe-argval frame))
                          val1)]
                     ;; Oops, they may be waiting to get back in here and set the result, let's get out quick.
                     [(stolen) 
                      ;; Let go of this so they can finish and mark it as done.
                      (mutex-release (shadowframe-mut frame))                
                      (find-and-steal-once!) ;; Meanwhile we should go try to make ourselves useful..                 
                      (waitloop)] ;; When we're done with that come back and see if our outsourced job is done.
                     ;; It was stolen and is now completed:
                     [else (pop!) 
                           (mutex-release (shadowframe-mut frame))
                           (op (shadowframe-argval frame) val1)]))
    
                 )))]))
    
      ;; Returns values in a list
      (define-syntax par
        (syntax-rules ()
          [(_ a b) (pcall list ((lambda (_) a) #f) b)]))
    
      (define-syntax parmv
        (syntax-rules ()
          [(_ a b) (pcall values ((lambda (_) a) #f) b)]))
    
      ;;================================================================================
    
      (init-par 4)
    
      (let ()
        (define (tree n)
          (if (fxzero? n) 1
              (pcall fx+ (tree (fx- n 1)) (tree (fx- n 1)))))
        (let ([n (time (tree test-depth))])
          (shutdown-par)
          n)))
    33554432)
  ($thread-check)
 ; make sure thread can return zero values
  (eqv?
    (let ([x #f])
      (fork-thread
        (lambda ()
          (call/cc
            (lambda (k)
              (set! x 'frob)
              (k)
              (set! x 'borf)))))
      (let f () (unless x ($yield) (f)))
      x)
    'frob)
  ($thread-check)
  (equal?
    (let ()
      (define n 10)
      (define m (make-mutex))
      (define c (make-condition))
      (define-ftype A (struct (x double) (y iptr)))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define ls '())
      (ftype-set! A (x) a 3.4)
      (ftype-set! A (y) a 1)
      (with-mutex m
        (do ([n n (fx- n 1)])
          ((fx= n 0))
          (fork-thread
            (lambda ()
              (define fat+
                (lambda (x y)
                  (if (zero? y)
                      x
                      (fat+ (1+ x) (1- y)))))
              (define fatfib
                (lambda (x)
                  (if (< x 2)
                      1
                      (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
              (do ([i (fx+ 1000 n) (fx- i 1)] [q (fatfib 4) (fatfib 4)])
                ((fx= i 0)
                 (with-mutex m
                   (set! ls (cons q ls))
                   (condition-signal c)))
                (if (odd? n)
                    (ftype-locked-incr! A (y) a)
                    (when (ftype-locked-decr! A (y) a)
                      (printf "woohoo!\n")))))))
        (let loop ()
          (if (equal? (length ls) n)
              (list (ftype-ref A (x) a) (ftype-ref A (y) a))
              (begin
                (condition-wait c m)
                (loop))))))
    '(3.4 -4))
  ($thread-check)
  (begin
    (load-thread-foreign-library!)
    #t)
  (equal?
    (let ()
      (define ff-locked-incr! (foreign-procedure "locked_incr" ((* iptr)) boolean))
      (define ff-locked-decr! (foreign-procedure "locked_decr" ((* iptr)) boolean))
      (define n 10)
      (define m (make-mutex))
      (define c (make-condition))
      (define-ftype A (struct (x double) (y iptr)))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define ls '())
      (ftype-set! A (x) a 3.4)
      (ftype-set! A (y) a 1)
      (ff-locked-incr! (ftype-&ref A (y) a)) ; try once in main thread, in case this errors
      (ff-locked-decr! (ftype-&ref A (y) a))
      (with-mutex m
        (do ([n n (fx- n 1)])
          ((fx= n 0))
          (fork-thread
            (lambda ()
              (define fat+
                (lambda (x y)
                  (if (zero? y)
                      x
                      (fat+ (1+ x) (1- y)))))
              (define fatfib
                (lambda (x)
                  (if (< x 2)
                      1
                      (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
              (do ([i (fx+ 1000 n) (fx- i 1)] [q (fatfib 10) (fatfib 10)])
                ((fx= i 0)
                 (with-mutex m
                   (set! ls (cons q ls))
                   (condition-signal c)))
                (if (odd? n)
                    (ff-locked-incr! (ftype-&ref A (y) a))
                    (when (ff-locked-decr! (ftype-&ref A (y) a))
                      (printf "hoowoo!\n")))))))
        (let loop ()
          (if (equal? (length ls) n)
              (list (ftype-ref A (x) a) (ftype-ref A (y) a))
              (begin
                (condition-wait c m)
                (loop))))))
    '(3.4 -4))
  ($thread-check)
  (eqv?
    (let ()
      (define n 10)
      (define m (make-mutex))
      (define c (make-condition))
      (define-ftype A (struct (x double) (y iptr)))
      (define a (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
      (define ls '())
      (ftype-set! A (x) a 3.5)
      (ftype-init-lock! A (y) a)
      (with-mutex m
        (do ([n n (fx- n 1)])
          ((fx= n 0))
          (fork-thread
            (lambda ()
              (define fat+
                (lambda (x y)
                  (if (zero? y)
                      x
                      (fat+ (1+ x) (1- y)))))
              (define fatfib
                (lambda (x)
                  (if (< x 2)
                      1
                      (fat+ (fatfib (1- x)) (fatfib (1- (1- x)))))))
              (do ([i (+ 1000 n) (fx- i 1)] [q (fatfib 4) (fatfib 4)])
                ((fx= i 0)
                 (with-mutex m
                   (set! ls (cons q ls))
                   (condition-signal c)))
                ; disable collections so we don't rendezvous for collection while holding the lock,
                ; leaving some other thread spinning hopelessly in ftype-spin-lock!
                (with-interrupts-disabled
                  (if (odd? i)
                      (let loop () (unless (ftype-lock! A (y) a) (printf "waiting\n") (loop)))
                      (ftype-spin-lock! A (y) a))
                  (memory-order-acquire)
                  (ftype-set! A (x) a ((if (odd? n) + -) (ftype-ref A (x) a) 1.0))
                  (memory-order-release)
                  (ftype-unlock! A (y) a))))))
        (let loop ()
          (if (equal? (length ls) n)
              (ftype-ref A (x) a)
              (begin
                (condition-wait c m)
                (loop))))))
    -1.5)
  ($thread-check)
  (parameterize ([collect-request-handler void])
    (define (expect-error what thunk)
      (guard (c [(and (message-condition? c)
                      (equal? (condition-message c)
                        (format "~a is defunct" what)))])
        (thunk)
        (error #f "error expected")))
    (let ([g (make-guardian #t)])
      (g (make-mutex))
      (collect)
      (let ([m (g)])
        (expect-error 'mutex (lambda () (mutex-acquire m)))
        (expect-error 'mutex (lambda () (mutex-release m)))
        (expect-error 'mutex (lambda () (condition-wait (make-condition) m))))
      (g (make-condition))
      (collect)
      (let ([c (g)])
        (expect-error 'condition (lambda () (condition-wait c (make-mutex))))
        (expect-error 'condition (lambda () (condition-broadcast c)))
        (expect-error 'condition (lambda () (condition-signal c))))))
 )

(mat make-thread-parameter
  (begin (define p (make-thread-parameter #f not)) #t)
  (p)
  (begin (p #f) (p))
  (begin (p #t) (not (p)))
  (begin (define q (make-thread-parameter #t)) #t)
  (q)
  (begin (q #f) (not (q)))
  (begin (q #t) (q))
  (or (= (optimize-level) 3)
      (guard (c [(and (message-condition? c)
                    (equal? (condition-message c) "~s is not a procedure")
                    (irritants-condition? c)
                    (equal? (condition-irritants c) (list 2)))])
      (make-thread-parameter 1 2)))
  (begin
    (define p
      (make-thread-parameter 5
        (lambda (x) (+ x 1))))
    #t)
  (eqv? (p) 6)
  (or (= (optimize-level) 3)
      (guard (c [(and (message-condition? c)
                      (equal? (condition-message c) "~s is not a number")
                      (irritants-condition? c)
                      (equal? (condition-irritants c) (list 'a)))])
        (p 'a)))
 )

(mat cas
 (begin
   (define (check container container-ref container-cas!)
     (let ([N 1000]
           [M 4]
           [done 0]
           [m (make-mutex)]
           [c (make-condition)])
       (define (bump)
         (let loop ([i 0])
           (unless (= i N)
             (let ([v (container-ref container)])
               (if (container-cas! container v (add1 v))
                   (loop (add1 i))
                   (loop i)))))
         (mutex-acquire m)
         (set! done (add1 done))
         (condition-signal c)
         (mutex-release m))
       (let loop ([j 0])
         (when (< j M)
           (fork-thread bump)
           (loop (add1 j))))
       (mutex-acquire m)
       (let loop ()
         (cond
          [(= done M)
           (mutex-release m)]
          [else
           (condition-wait c m)
           (loop)]))
       (= (container-ref container) (* M N))))
   (check (box 0) unbox box-cas!))
 (check (vector 1 0 2) (lambda (v) (vector-ref v 1)) (lambda (v o n) (vector-cas! v 1 o n))))

(mat rendezvous-in-main
  ;; make sure that every collect rendezvous triggers the handler in the
  ;; main thread
  (let ([done? #f]
        [gc-ids (list (get-thread-id))]
        [gc-count 0]
        [m #%$tc-mutex] ; using tc-mutex means main thread is not deactivated
        [c (make-condition)])
    (parameterize ([collect-request-handler
                    (let ([orig (collect-request-handler)])
                      (lambda ()
                        (set! gc-count (add1 gc-count))
                        (unless (memv (get-thread-id) gc-ids)
                          (set! gc-ids (cons (get-thread-id) gc-ids)))
                        (orig)))])
      (let loop ([i 4])
        (unless (= i 0)
          (fork-thread (lambda () (let loop ()
                                    (unless (with-mutex m
                                               (unless done?
                                                 (condition-wait c m))
                                               done?)
                                      (collect-rendezvous)
                                      (loop)))))
          (loop (sub1 i))))
      (let loop ()
        (unless (> gc-count 1000)
          (with-mutex m (condition-broadcast c))
          (collect-rendezvous)
          (loop)))
      (with-mutex m
        (set! done? #t)
        (condition-broadcast c))
      (equal? gc-ids (list (get-thread-id)))))
  )

(mat memory-consistency
  (equal? (memory-order-acquire) (void))
  (equal? (memory-order-release) (void))
  ;; Try to make a thread see a partially constructed box
  (let ([ids '(one two three four)])
    (let ([m (make-mutex)]
          [c (make-condition)]
          [ok? #t]
          [running (length ids)]
          [v (make-vector 1000 (box (car ids)))])
      (let loop ([i running])
        (unless (= i 0)
          (fork-thread (lambda ()
                         (let ([id (list-ref ids (sub1 i))]
                               [failed? #f])
                           (let loop ([j 10000])
                             (cond
                               [(fx= j 0)
                                (mutex-acquire m)
                                (set! running (sub1 running))
                                (condition-signal c)
                                (set! ok? (and ok? (not failed?)))
                                (mutex-release m)]
                               [else
                                (let loop ([i 0])
                                  (unless (fx= i (vector-length v))
                                    (let ([b (vector-ref v i)])
                                      (unless (and (box? b)
                                                   (memq (unbox b) ids))
                                        (set! failed? #t)))
                                    (vector-set! v i (box id))
                                    (loop (fx+ i 1))))
                                (loop (fx- j 1))])))))
          (loop (sub1 i))))
      (mutex-acquire m)
      (let loop ()
        (cond
          [(not (zero? running))
           (condition-wait c m)
           (loop)]
          [else
           (mutex-release m)]))
      ok?))
)

(mat parallel-gc-regression
  (equal?
   ;; Create threads that refer to data owned by other threads, and try to
   ;; mix things up to see if we can provoke a problem in parallel collection.
   (parameterize ([collect-request-handler
                   (lambda ()
                     (let ([g (collect-maximum-generation)])
                       (collect (- g 1) (min 1 (- g 1)) (- g 1))))])
     (let loop ([j 50])
       (unless (= j 0)
         (let* ([N 8]
                [DEPTH 24]
                [common (make-vector N)])

           (define (spin i)
             (define (fib n)
               (cond
                 [(= n 0) #t]
                 [(= n 1) #f]
                 [else (cons (fib (- n 1)) (fib (- n 2)))]))
             (vector-set! common i (fib DEPTH))
             (when (= i (sub1 N))
               (collect-rendezvous)))

           (define (work i)
             (lambda ()
               (thread-preserve-ownership!)
               (set-virtual-register! 0 (make-vector 128))
               (vector-set! (virtual-register 0) 0 (make-eq-hashtable))
               (map (lambda (j)
                      (let ([r (random N)])
                        (vector-set! common r (make-vector 10 ))))
                    '(1 2 3))
               (map (lambda (j)
                      (let ([r (random N)])
                        (set-virtual-register! j (vector-ref common r))))
                    '(1 2 3))
               (spin i)
               (unless (hashtable? (vector-ref (virtual-register 0) 0))
                 (error 'oops "something went wrong"))))

           (define ths
             (let loop ([i 0])
               (cond
                 [(= i N) '()]
                 [else
                  (let ([th (fork-thread (work i))])
                    (vector-set! common i th)
                    (cons th (loop (add1 i))))])))

           (for-each thread-join ths)

           (loop (sub1 j))))))
   (void))
)

(mat native-fiber-foreign-support
  (begin (load-thread-foreign-library!) #t))

(define native-fiber-test-hook-arm
  (foreign-procedure "(cs)native_fiber_test_hook_arm" (iptr iptr iptr) void))
(define native-fiber-test-hook-release
  (foreign-procedure "(cs)native_fiber_test_hook_release" () void))
(define native-fiber-test-hook-hit
  (foreign-procedure "(cs)native_fiber_test_hook_hit" () iptr))
(define native-fiber-test-hook-saw-gc
  (foreign-procedure "(cs)native_fiber_test_hook_saw_gc" () iptr))
(define native-fiber-test-hook-request-exit
  (foreign-procedure "(cs)native_fiber_test_hook_request_exit" () void))
(define native-fiber-test-hook-saw-exit
  (foreign-procedure "(cs)native_fiber_test_hook_saw_exit" () iptr))
(define native-fiber-test-hook-reset
  (foreign-procedure "(cs)native_fiber_test_hook_reset" () void))
(define native-fiber-gc-test-phase-reset
  (foreign-procedure "(cs)gc_test_phase_reset" () void))
(define native-fiber-gc-test-phase-arm
  (foreign-procedure "(cs)gc_test_phase_arm" (iptr iptr) void))
(define native-fiber-gc-test-phase-mask
  (foreign-procedure "(cs)gc_test_phase_mask" () iptr))
(define native-fiber-gc-test-all-phases #b11111)

(define (native-fiber-wait-for-hook point)
  (let loop ([attempts 10000])
    (cond
      [(fx= (native-fiber-test-hook-hit) point) #t]
      [(fx= attempts 0) #f]
      [else
       (sleep (make-time 'time-duration 100000 0))
       (loop (fx- attempts 1))])))

(define (native-fiber-string-contains? string needle)
  (let ([string-length (string-length string)]
        [needle-length (string-length needle)])
    (let loop ([start 0])
      (and (fx<= (fx+ start needle-length) string-length)
           (or (string=?
                 (substring string start (fx+ start needle-length))
                 needle)
               (loop (fx+ start 1)))))))

(define (native-fiber-test-scheme-command)
  (or (getenv "CHEZ_MAT_SCHEME_COMMAND")
      (patch-exec-path *scheme*)))

(define (native-fiber-subprocess-death-test forms survived-marker expected)
  (let-values ([(to-stdin from-stdout from-stderr pid)
                (open-process-ports
                  (format "~a -q" (native-fiber-test-scheme-command))
                  (buffer-mode block)
                  (native-transcoder))])
    (for-each
      (lambda (form) (pretty-print form to-stdin))
      (append forms `((display ,survived-marker))))
    (close-port to-stdin)
    (let ([stdout (slurp-port from-stdout)]
          [stderr (slurp-port from-stderr)])
      (close-port from-stdout)
      (close-port from-stderr)
      (and (not (native-fiber-string-contains? stdout survived-marker))
           (native-fiber-string-contains? stderr expected)
           #t))))

(define (native-fiber-run-hook-point point gc-generation)
  (let ([task #f]
        [scheduler #f]
        [result #f]
        [claim-lock (make-mutex)]
        [claim-condition (make-condition)]
        [claim-attempted? #f]
        [claim-exit? #f])
    (parameterize
      ([collect-request-handler
        (if gc-generation
            (lambda () (collect gc-generation))
            (collect-request-handler))])
      (dynamic-wind
        (lambda ()
          (native-fiber-test-hook-arm point (if gc-generation 2 1) 0))
        (lambda ()
          (let ([worker
                 (fork-thread
                   (lambda ()
                     (set! scheduler (#3%$native-fiber-adopt #b1000))
                     (set! task
                       (#3%$native-fiber-create
                         (lambda () 'done)
                         (native-fiber-return-handler scheduler)
                         #b1100))
                     (#3%$native-fiber-try-claim! task)
                     (set! result
                       (#3%$native-fiber-switch scheduler task 'start))))])
            (if (not (native-fiber-wait-for-hook point))
                (begin
                  (native-fiber-test-hook-release)
                  (thread-join worker)
                  #f)
                (let* ([foreign-claim #t]
                       [claim-worker
                        (fork-thread
                          (lambda ()
                            (#3%$native-fiber-adopt #b1000)
                            (set! foreign-claim
                              (#3%$native-fiber-try-claim! task))
                            ;; Thread teardown may rendezvous with every
                            ;; worker. Keep this worker registered until the
                            ;; deliberately paused transition is released.
                            (with-mutex claim-lock
                              (set! claim-attempted? #t)
                              (condition-broadcast claim-condition)
                              (let wait ()
                                (unless claim-exit?
                                  (condition-wait claim-condition claim-lock)
                                  (wait))))))])
                  (with-mutex claim-lock
                    (let wait ()
                      (unless claim-attempted?
                        (condition-wait claim-condition claim-lock)
                        (wait))))
                  (when gc-generation
                    (native-fiber-gc-test-phase-reset))
                  (let* ([collector
                         (and gc-generation
                              (fork-thread
                                (lambda ()
                                  (parameterize ([#%$enable-check-heap #t])
                                    (collect-rendezvous)))))]
                         [gc-phase-mask
                          (begin
                            (when collector (thread-join collector))
                            (and gc-generation
                                 (native-fiber-gc-test-phase-mask)))]
                         [gc-overlap?
                          (begin
                            (or (not gc-generation)
                                (and (fx= (native-fiber-test-hook-hit) point)
                                     (fx= (native-fiber-test-hook-saw-gc) 1)
                                     (fx= gc-phase-mask
                                          native-fiber-gc-test-all-phases))))])
                    (native-fiber-test-hook-release)
                    (with-mutex claim-lock
                      (set! claim-exit? #t)
                      (condition-broadcast claim-condition))
                    (thread-join claim-worker)
                    (thread-join worker)
                    (and (not foreign-claim)
                         gc-overlap?
                         (equal? result '(return . done))
                         (eq? (#3%$native-fiber-state task) 'finished)
                         (eq? (#3%$native-fiber-state scheduler) 'finished)))))))
        (lambda ()
          (when gc-generation
            ;; Disarm phase recording even if collection or validation exits.
            (native-fiber-gc-test-phase-mask))
          (native-fiber-test-hook-reset)
          (with-mutex claim-lock
            (set! claim-exit? #t)
            (condition-broadcast claim-condition)))))))

(define (native-fiber-run-hook-preemption point)
  (let ([task #f]
        [scheduler #f]
        [first-result #f]
        [final-result #f]
        [handler-count 0]
        [requested? #f])
    (dynamic-wind
      (lambda () (native-fiber-test-hook-arm point 1 6))
      (lambda ()
        (let ([worker
               (fork-thread
                 (lambda ()
                   (guard (condition
                            [else
                             (set! final-result
                               (cons 'exception
                                 (with-output-to-string
                                   (lambda ()
                                     (display-condition condition)))))])
                     (parameterize
                       ([timer-interrupt-handler
                         (lambda ()
                           (set! handler-count (fx+ handler-count 1))
                           (set! requested?
                             (#3%$native-fiber-preempt
                               scheduler 'transition-preempted)))])
                       (set! scheduler (#3%$native-fiber-adopt #b1000))
                       (set! task
                         (#3%$native-fiber-create
                           (lambda () 'done)
                           (native-fiber-return-handler scheduler)
                           #b1100))
                       (#3%$native-fiber-try-claim! task)
                       (set! first-result
                         (#3%$native-fiber-switch scheduler task 'start))
                       (when (eq? first-result 'transition-preempted)
                         (#3%$native-fiber-try-claim! task)
                         (set! final-result
                           (#3%$native-fiber-switch
                             scheduler task 'resume)))))))])
          (let ([hit? (native-fiber-wait-for-hook point)])
            (native-fiber-test-hook-release)
            (thread-join worker)
            (let ([snapshot
                   (list hit? requested? handler-count first-result final-result
                     (and task (#3%$native-fiber-state task))
                     (and scheduler (#3%$native-fiber-state scheduler)))])
              (unless (equal? snapshot
                        '(#t #t 1 transition-preempted
                          (return . done) finished finished))
                (error 'native-fiber-run-hook-preemption
                  "unexpected transition-preemption result"
                  point snapshot))
              #t))))
      native-fiber-test-hook-reset)))

(define (native-fiber-run-hook-exit point)
  (let ([task #f] [scheduler #f] [result #f])
    (dynamic-wind
      (lambda () (native-fiber-test-hook-arm point 3 0))
      (lambda ()
        (let ([worker
               (fork-thread
                 (lambda ()
                   (set! scheduler (#3%$native-fiber-adopt #b1000))
                   (set! task
                     (#3%$native-fiber-create
                       (lambda () 'done)
                       (native-fiber-return-handler scheduler)
                       #b1100))
                   (#3%$native-fiber-try-claim! task)
                   (set! result
                     (#3%$native-fiber-switch scheduler task 'start))))])
          (let ([hit? (native-fiber-wait-for-hook point)])
            (if hit?
                (native-fiber-test-hook-request-exit)
                (native-fiber-test-hook-release))
            (thread-join worker)
            (and hit?
                 (fx= (native-fiber-test-hook-saw-exit) 1)
                 (equal? result '(return . done))
                 (eq? (#3%$native-fiber-state task) 'finished)
                 (eq? (#3%$native-fiber-state scheduler) 'finished)))))
      native-fiber-test-hook-reset)))

(define (native-fiber-death-test point action expected)
  (native-fiber-subprocess-death-test
    `((define arm
          (foreign-procedure "(cs)native_fiber_test_hook_arm"
            (iptr iptr iptr) void))
        (define scheduler (#3%$native-fiber-adopt #b1000))
        (define task #f)
        (set! task
          (#3%$native-fiber-create
            (lambda () 'done)
            (lambda (fiber outcome)
              (#3%$native-fiber-try-claim! scheduler)
              (#3%$native-fiber-finish fiber scheduler outcome))
            #b1100))
        (arm ,point 0 ,action)
        (#3%$native-fiber-try-claim! task)
        (#3%$native-fiber-switch scheduler task 'start))
    "native-fiber-death-test-survived"
    expected))

(define (native-fiber-claim-corruption-death-test corruption expected)
  (native-fiber-subprocess-death-test
    `((define fiber (#3%$native-fiber-create values values #b100))
      (case ',corruption
        [(missing-context)
         (#3%$record-set! fiber ,native-fiber-context-field-index #f)]
        [(generic-context)
         (call/cc
           (lambda (context)
             (#3%$record-set! fiber ,native-fiber-context-field-index context)))]
        [(stable-scratch)
         (#3%$record-set! fiber ,native-fiber-incoming-source-field-index fiber)]
        [(bad-flags)
         (#3%$record-set! fiber ,native-fiber-flags-field-index 0)]
        [(mixed-entry-roots)
         (#3%$record-set! fiber ,native-fiber-entry-field-index #f)])
      (#3%$native-fiber-try-claim! fiber))
    "native-fiber-claim-corruption-survived"
    expected))

(mat native-fiber-claimed-state-corruption
  (native-fiber-claim-corruption-death-test
    'missing-context "inconsistent ownership or context")
  (native-fiber-claim-corruption-death-test
    'generic-context "inconsistent ownership or context")
  (native-fiber-claim-corruption-death-test
    'stable-scratch "retains transition state")
  (native-fiber-claim-corruption-death-test
    'bad-flags "invalid flags")
  (native-fiber-claim-corruption-death-test
    'mixed-entry-roots "inconsistent ownership or context"))

(mat native-fiber-transition-fault-points
  (andmap (lambda (point) (native-fiber-run-hook-point point #f))
    '(1 2 3 4)))

(define native-fiber-test-generations
  (let ([maximum (collect-maximum-generation)])
    (cond
      [(fx= maximum 0) '(0)]
      [(fx= maximum 1) '(0 1)]
      [else (list 0 (fxquotient maximum 2) maximum)])))

(define (native-fiber-collect-phase-mask thunk)
  (dynamic-wind
    native-fiber-gc-test-phase-reset
    (lambda ()
      (thunk)
      (native-fiber-gc-test-phase-mask))
    native-fiber-gc-test-phase-mask))

(mat native-fiber-gc-phase-coverage
  (fx= (native-fiber-collect-phase-mask
         (lambda ()
           (parameterize ([#%$enable-check-heap #t])
             (collect (collect-maximum-generation)))))
       native-fiber-gc-test-all-phases)
  (fx= (native-fiber-collect-phase-mask
         (lambda ()
           (parameterize ([#%$enable-check-heap #t]
                          [enable-object-counts #t])
             (collect (collect-maximum-generation)))))
       native-fiber-gc-test-all-phases)
  (fx= (native-fiber-collect-phase-mask
         (lambda ()
           (parameterize ([#%$enable-check-heap #t])
             (collect 0))))
       native-fiber-gc-test-all-phases))

(define (native-fiber-gc-phase-death-test phase)
  (native-fiber-subprocess-death-test
    `((define enable-check
          (foreign-procedure "(cs)enable_check_heap" (boolean) void))
        (define arm
          (foreign-procedure "(cs)gc_test_phase_arm" (iptr iptr) void))
        (enable-check #t)
        (arm ,phase 1)
        (collect (collect-maximum-generation)))
    "native-fiber-gc-phase-death-test-survived"
    "gc phase fault injected"))

(mat native-fiber-gc-phase-fault-injection
  (andmap native-fiber-gc-phase-death-test '(0 1 2 3 4)))

(define (checkheap-mark-death-test action expected)
  (native-fiber-subprocess-death-test
    `((define enable-check
          (foreign-procedure "(cs)enable_check_heap" (boolean) void))
        (define arm
          (foreign-procedure "(cs)checkheap_test_mark_fault"
            (scheme-object iptr) void))
        (define root #f)
        (enable-check #t)
        ,(case action
           [(1)
            '(begin
               (set! root (vector (list 'reachable)))
               (arm (vector-ref root 0) 1))]
           [(2)
            '(let ([unreachable (list 'unreachable)])
               (arm unreachable 2))])
        (collect 0)
        (keep-live root))
    "checkheap-mark-death-test-survived"
    expected))

(mat checkheap-bidirectional-mark-faults
  (checkheap-mark-death-test
    1 "checkmark found reachable object omitted")
  (checkheap-mark-death-test
    2 "checkmark found unreachable object retained"))

(define (checkheap-remembered-death-test)
  (native-fiber-subprocess-death-test
    `((define enable-check
          (foreign-procedure "(cs)enable_check_heap" (boolean) void))
        (define arm
          (foreign-procedure "(cs)checkheap_test_remembered_fault"
            (scheme-object iptr) void))
        (define source (vector #f))
        (define target #f)
        (enable-check #t)
        (collect 0 1 1)
        (set! target (list 'young-target))
        (vector-set! source 0 target)
        (arm source 0)
        (collect 0)
        (keep-live source))
    "checkheap-remembered-death-test-survived"
    "rebuilt remembered set found missing card"))

(mat checkheap-remembered-set-fault
  (checkheap-remembered-death-test))

(define (checkheap-double-arm-death-test kind)
  (native-fiber-subprocess-death-test
    (case kind
      [(mark)
       `((define arm
           (foreign-procedure "(cs)checkheap_test_mark_fault"
             (scheme-object iptr) void))
         (define target (list 'target))
         (arm target 1)
         (arm target 1))]
      [(remembered)
       `((define arm
           (foreign-procedure "(cs)checkheap_test_remembered_fault"
             (scheme-object iptr) void))
         (define source (vector #f))
         (arm source 0)
         (arm source 0))])
    "checkheap-double-arm-death-test-survived"
    (case kind
      [(mark) "checkheap mark fault is already armed"]
      [(remembered) "checkheap remembered-set fault is already armed"])))

(mat checkheap-fault-single-arm
  (checkheap-double-arm-death-test 'mark)
  (checkheap-double-arm-death-test 'remembered))

(define (native-fiber-census-death-test corruption)
  (native-fiber-subprocess-death-test
    `((define enable-check
          (foreign-procedure "(cs)enable_check_heap" (boolean) void))
        (define fiber-a #f)
        (define fiber-b #f)
        (case ',corruption
          [(missing-current)
           (set! fiber-a (#3%$native-fiber-adopt 0))
           (#3%$current-native-fiber #f)]
          [(missing-claim)
           (set! fiber-a
             (#3%$native-fiber-create values values #b100))
           (#3%$native-fiber-try-claim! fiber-a)
           (#3%$native-fiber-claimed #f)
           (#3%$native-fiber-claimed-control #f)]
          [(bad-claim-rollback)
           (set! fiber-a
             (#3%$native-fiber-create values values #b100))
           (#3%$native-fiber-try-claim! fiber-a)
           (#3%$native-fiber-claimed-control (fxsll 1 3))]
          [(missing-pinned)
           (set! fiber-a
             (#3%$native-fiber-create values values #b001))
           (#3%$native-fiber-try-claim! fiber-a)
           (#3%$tc-field 'native-fiber-pinned-head (#3%$tc) #f)]
          [(aliased-context)
           (set! fiber-a
             (#3%$native-fiber-create values values #b100))
           (set! fiber-b
             (#3%$native-fiber-create values values #b100))
           (#3%$record-set! fiber-b ,native-fiber-context-field-index
             (#3%$record-ref fiber-a ,native-fiber-context-field-index))]
          [(bad-flags)
           (set! fiber-a
             (#3%$native-fiber-create values values #b100))
           (#3%$record-set! fiber-a ,native-fiber-flags-field-index #b101)]
          [(stable-scratch)
           (set! fiber-a
             (#3%$native-fiber-create values values #b100))
           (#3%$record-set! fiber-a ,native-fiber-incoming-payload-field-index
             (vector 'retained))]
          [(invalid-owner)
           (set! fiber-a
             (#3%$native-fiber-create values values #b100))
           (#3%$record-set! fiber-a ,native-fiber-control-field-index
             (fxlogor 4 (fxsll 999 3)))]
          [(scheduler-new)
           (set! fiber-a
             (#3%$native-fiber-create values values #b100))
           (#3%$record-set! fiber-a ,native-fiber-flags-field-index #b011)]
          [(mixed-claim-roots)
           (set! fiber-a
             (#3%$native-fiber-create values values #b100))
           (#3%$native-fiber-try-claim! fiber-a)
           (#3%$record-set! fiber-a ,native-fiber-entry-field-index #f)]
          [(finished-roots)
           (set! fiber-a
             (#3%$native-fiber-create values values #b100))
           (#3%$record-set! fiber-a ,native-fiber-control-field-index 6)]
          [(pinned-cycle)
           (set! fiber-a
             (#3%$native-fiber-create values values #b001))
           (#3%$native-fiber-try-claim! fiber-a)
           (#3%$record-set! fiber-a ,native-fiber-pinned-next-field-index fiber-a)]
          [(stale-preempt-payload)
           (#3%$tc-field 'native-fiber-preempt-payload
             (#3%$tc) 'stale)])
        (enable-check #t)
        (collect (collect-maximum-generation)))
    "native-fiber-census-death-test-survived"
    "native-fiber census"))

(mat native-fiber-global-census-invariants
  (andmap native-fiber-census-death-test
    '(missing-current missing-claim bad-claim-rollback
      missing-pinned aliased-context bad-flags stable-scratch
      invalid-owner scheduler-new mixed-claim-roots finished-roots
      pinned-cycle stale-preempt-payload)))

(mat native-fiber-concurrent-gc-handshake
  (andmap
    (lambda (generation)
      (andmap
        (lambda (point)
          (native-fiber-run-hook-point point generation))
        '(1 2 3 4)))
    native-fiber-test-generations))

(mat native-fiber-transition-preemption
  (andmap native-fiber-run-hook-preemption '(1 2 3 4)))

(mat native-fiber-transition-worker-exit
  (andmap native-fiber-run-hook-exit '(1 2 3 4)))

(define (native-fiber-randomized-transition-stress seed rounds)
  (define modulus 2147483647)
  (define gc-mode-count (length native-fiber-test-generations))
  (define mode-count (fx+ gc-mode-count 3))
  (define (next state)
    (modulo (+ (* state 48271) 1) modulus))
  (let loop ([state (modulo seed modulus)] [iteration 0])
    (if (fx= iteration rounds)
        #t
        (let* ([point-state (next state)]
               [mode-state (next point-state)]
               [point (+ (modulo point-state 4) 1)]
               [mode (modulo mode-state mode-count)]
               [scenario
                (cond
                  [(fx= mode 0) 'transition]
                  [(fx<= mode gc-mode-count)
                   (cons 'gc
                     (list-ref native-fiber-test-generations (fx- mode 1)))]
                  [(fx= mode (fx+ gc-mode-count 1)) 'preemption]
                  [else 'worker-exit])]
               [ok?
                (cond
                  [(eq? scenario 'transition)
                   (native-fiber-run-hook-point point #f)]
                  [(pair? scenario)
                   (native-fiber-run-hook-point point (cdr scenario))]
                  [(eq? scenario 'preemption)
                   (native-fiber-run-hook-preemption point)]
                  [else (native-fiber-run-hook-exit point)])])
          (unless ok?
            (error 'native-fiber-randomized-transition-stress
              "transition scenario failed"
              seed iteration point scenario))
          (loop mode-state (fx+ iteration 1))))))

(define (native-fiber-stress-integer name default minimum)
  (let ([value (getenv name)])
    (if value
        (let ([value (string->number value)])
          (unless (and (integer? value) (exact? value) (>= value minimum))
            (error 'native-fiber-stress-integer
              "invalid native-fiber stress setting" name value))
          value)
        default)))

(mat native-fiber-randomized-transition-stress
  (native-fiber-randomized-transition-stress
    (native-fiber-stress-integer
      "CHEZ_NATIVE_FIBER_STRESS_SEED" 1592594996 0)
    (native-fiber-stress-integer
      "CHEZ_NATIVE_FIBER_STRESS_ROUNDS" 24 1)))

(mat native-fiber-transition-death-tests
  (native-fiber-death-test 1 1 "native-fiber source publication raced")
  (native-fiber-death-test 3 2 "native-fiber commit publication raced")
  (native-fiber-death-test 2 3 "native-fiber target return point is invalid")
  (native-fiber-death-test 1 4 "native-fiber stack ownership overlaps")
  (native-fiber-death-test 4 5
    "native-fiber source scratch corrupted before commit"))

(mat native-fiber-test-hook-single-arm
  (native-fiber-subprocess-death-test
    `((define arm
        (foreign-procedure "(cs)native_fiber_test_hook_arm"
          (iptr iptr iptr) void))
      (arm 1 0 0)
      (arm 2 0 0))
    "native-fiber-test-hook-double-arm-survived"
    "native-fiber test hook is already armed"))

(mat native-fiber-repeated-claim-contention
  (let rounds ([round 0])
    (if (fx= round 24)
        #t
        (let* ([worker-count 6]
               [task (#3%$native-fiber-create values values #b100)]
               [m (make-mutex)]
               [c (make-condition)]
               [ready 0]
               [done 0]
               [go? #f]
               [release? #f]
               [wins 0]
               [threads
                (let loop ([i 0])
                  (if (fx= i worker-count)
                      '()
                      (cons
                        (fork-thread
                          (lambda ()
                            (#3%$native-fiber-adopt 0)
                            (with-mutex m
                              (set! ready (fx+ ready 1))
                              (condition-broadcast c)
                              (let wait ()
                                (unless go?
                                  (condition-wait c m)
                                  (wait))))
                            (let ([won? (#3%$native-fiber-try-claim! task)])
                              (with-mutex m
                                (when won? (set! wins (fx+ wins 1)))
                                (set! done (fx+ done 1))
                                (condition-broadcast c)
                                (when won?
                                  (let wait ()
                                    (unless release?
                                      (condition-wait c m)
                                      (wait)))))
                              (when won?
                                (#3%$native-fiber-release-claim! task)))))
                        (loop (fx+ i 1)))))])
          (with-mutex m
            (let wait ()
              (unless (fx= ready worker-count)
                (condition-wait c m)
                (wait)))
            (set! go? #t)
            (condition-broadcast c)
            (let wait ()
              (unless (fx= done worker-count)
                (condition-wait c m)
                (wait)))
            (set! release? #t)
            (condition-broadcast c))
          (for-each thread-join threads)
          (and (fx= wins 1)
               (eq? (#3%$native-fiber-state task) 'new)
               (not (#3%$native-fiber-owner task))
               (rounds (fx+ round 1)))))))

(mat native-fiber-contended-lifecycle
  (let* ([main-scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [current-scheduler main-scheduler]
         [task #f]
         [round-count 12]
         [worker-count 6])
    (define (race-to-resume expected)
      (let ([m (make-mutex)]
            [c (make-condition)]
            [ready 0]
            [attempted 0]
            [go? #f]
            [wins 0]
            [transfer #f])
        (let ([threads
               (let loop ([i 0])
                 (if (fx= i worker-count)
                     '()
                     (cons
                       (fork-thread
                         (lambda ()
                           (let ([scheduler (#3%$native-fiber-adopt 0)])
                             (with-mutex m
                               (set! ready (fx+ ready 1))
                               (condition-broadcast c)
                               (let wait ()
                                 (unless go?
                                   (condition-wait c m)
                                   (wait))))
                             (let ([won?
                                    (#3%$native-fiber-try-claim! task)])
                               (with-mutex m
                                 (when won? (set! wins (fx+ wins 1)))
                                 (set! attempted (fx+ attempted 1))
                                 (condition-broadcast c)
                                 (when won?
                                   (let wait ()
                                     (unless (fx= attempted worker-count)
                                       (condition-wait c m)
                                       (wait)))))
                               (when won?
                                 (set! current-scheduler scheduler)
                                 (set! transfer
                                   (#3%$native-fiber-switch
                                     scheduler task expected)))))))
                       (loop (fx+ i 1)))))])
          (with-mutex m
            (let wait ()
              (unless (fx= ready worker-count)
                (condition-wait c m)
                (wait)))
            (set! go? #t)
            (condition-broadcast c))
          (for-each thread-join threads)
          (and (fx= wins 1)
               (if (fx= expected round-count)
                   (equal? transfer '(return . done))
                   (fx= transfer expected))))))
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (let loop ([i 0])
            (if (fx= i round-count)
                'done
                (begin
                  (#3%$native-fiber-try-claim! current-scheduler)
                  (#3%$native-fiber-switch task current-scheduler i)
                  (loop (fx+ i 1))))))
        (native-fiber-dynamic-return-handler (lambda () current-scheduler))
        #b100))
    (#3%$native-fiber-try-claim! task)
    (and (fx= (#3%$native-fiber-switch main-scheduler task 'start) 0)
         (let loop ([expected 1])
           (and (race-to-resume expected)
                (if (fx= expected round-count)
                    #t
                    (loop (fx+ expected 1)))))
         (eq? (#3%$native-fiber-state task) 'finished)
         (not (#3%$native-fiber-owner task)))))

(mat native-fiber-claimed-gc-root
  (let* ([task (#3%$native-fiber-create values values #b100)]
         [context-weak
          (weak-cons
            (#3%$record-ref task native-fiber-context-field-index) #f)]
         [m (make-mutex)]
         [c (make-condition)]
         [claimed? #f]
         [release? #f]
         [worker
          (fork-thread
            (lambda ()
              (#3%$native-fiber-adopt 0)
              (#3%$native-fiber-try-claim! task)
              (with-mutex m
                (set! claimed? #t)
                (condition-broadcast c)
                (let wait ()
                  (unless release?
                    (condition-wait c m)
                    (wait))))
              (#3%$native-fiber-release-claim! task)))])
    (with-mutex m
      (let wait ()
        (unless claimed?
          (condition-wait c m)
          (wait))))
    (parameterize ([#%$enable-check-heap #t])
      (collect-rendezvous))
    (let ([context (car context-weak)])
      (with-mutex m
        (set! release? #t)
        (condition-broadcast c))
      (thread-join worker)
      (and context
           (eq? (#3%$native-fiber-state task) 'new)
           (not (#3%$native-fiber-owner task))))))

(mat native-fiber-worker-exit-cleanup
  (let ([task #f] [scheduler #f])
    (thread-join
      (fork-thread
        (lambda ()
          (set! scheduler (#3%$native-fiber-adopt #b1000))
          (set! task (#3%$native-fiber-create values values #b1100))
          (#3%$native-fiber-try-claim! task))))
    (and (eq? (#3%$native-fiber-state task) 'new)
         (not (#3%$native-fiber-owner task))
         (eq? (#3%$native-fiber-state scheduler) 'finished)))
  (let ([task #f] [scheduler #f] [cached-weak #f])
    (thread-join
      (fork-thread
        (lambda ()
          (set! scheduler (#3%$native-fiber-adopt #b1000))
          (set! task
            (#3%$native-fiber-create
              (lambda ()
                (#3%$native-fiber-try-claim! scheduler)
                (#3%$native-fiber-switch task scheduler 'parked)
                'unreachable)
              (native-fiber-return-handler scheduler)
              #b1001))
          (#3%$native-fiber-try-claim! task)
          (#3%$native-fiber-switch scheduler task 'start)
          ;; Leave a parked pinned task in the claimed handoff slot. Worker
          ;; teardown must roll the claim back before retiring the task.
          (#3%$native-fiber-try-claim! task)
          (let ([cached (#3%$tc-field 'cached-frame (#3%$tc))])
            (when cached (set! cached-weak (weak-cons cached #f)))))))
    (collect (collect-maximum-generation))
    (and (eq? (#3%$native-fiber-state task) 'finished)
         (not (#3%$native-fiber-owner task))
         (eq? (#3%$native-fiber-state scheduler) 'finished)
         (or (not cached-weak) (bwp-object? (car cached-weak)))))
  (let ([payload (vector 'worker-exit-preemption-root)]
        [payload-weak #f]
        [target-weak #f])
    (set! payload-weak (weak-cons payload #f))
    (thread-join
      (fork-thread
        (lambda ()
          (let ([scheduler (#3%$native-fiber-adopt #b1000)])
            (set! target-weak (weak-cons scheduler #f))
            ;; Prevent the ordinary event epilogue from consuming this stale
            ;; request before the fork-thread exit path sanitizes its roots.
            (disable-interrupts)
            (#3%$tc-field 'native-fiber-preempt-payload (#3%$tc) payload)
            (#3%$tc-field 'native-fiber-preempt-target
              (#3%$tc) scheduler)))))
    (set! payload #f)
    (collect (collect-maximum-generation))
    (and (bwp-object? (car payload-weak))
         (bwp-object? (car target-weak)))))

(mat native-fiber-parallel-collector
  (let* ([n 4]
         [m (make-mutex)]
         [ready-condition (make-condition)]
         [resume-condition (make-condition)]
         [ready 0]
         [resume? #f]
         [tasks (make-vector n #f)]
         [announced (make-vector n #f)]
         [disable-counts (make-vector n #f)]
         [results (make-vector n #f)])
    (define (announce-locked i)
      (unless (vector-ref announced i)
        (vector-set! announced i #t)
        (set! ready (fx+ ready 1))
        (condition-signal ready-condition)))
    (define (announce i)
      (with-mutex m (announce-locked i)))
    (define (worker i)
      (guard (condition
               [else
                (vector-set! results i condition)
                (announce i)])
        (let ([scheduler (#3%$native-fiber-adopt 0)]
              [task #f])
          (set! task
            (#3%$native-fiber-create
              (lambda ()
                (let down ([depth 3000])
                  (if (fx= depth 0)
                      (begin
                        (#3%$native-fiber-try-claim! scheduler)
                        (#3%$native-fiber-switch task scheduler i)
                        i)
                      (fx+ 1 (down (fx- depth 1))))))
              (native-fiber-return-handler scheduler)
              #b100))
          (#3%$native-fiber-try-claim! task)
          (let ([yielded (#3%$native-fiber-switch scheduler task 'start)])
            (vector-set! tasks i task)
            (with-mutex m
              ;; Publish readiness while holding the same mutex that
              ;; condition-wait releases. The main thread cannot collect
              ;; until this worker has actually entered the wait protocol.
              (vector-set! disable-counts i
                (#3%$tc-field 'disable-count (#3%$tc)))
              (announce-locked i)
              (let wait ()
                (unless resume?
                  (condition-wait resume-condition m)
                  (wait))))
            (vector-set! results i
              (and (fx= yielded i)
                   (eq? (#3%$native-fiber-state task) 'parked)))))))
    (let ([threads
           (let loop ([i 0])
             (if (fx= i n)
                 '()
                 (cons (fork-thread (lambda () (worker i)))
                       (loop (fx+ i 1)))))])
      (with-mutex m
        (let wait ()
          (unless (fx= ready n)
            (condition-wait ready-condition m)
            (wait))))
      (unless (andmap (lambda (count) (fx= count 0))
                      (vector->list disable-counts))
        (error 'native-fiber-parallel-collector
          "unbalanced interrupt-disable counts" disable-counts))
      ;; Rendezvous with every active runtime thread before inspecting the
      ;; parked native-fiber stacks in the root set.
      (parameterize ([#%$enable-check-heap #t])
        (collect-rendezvous))
      (with-mutex m
        (set! resume? #t)
        (condition-broadcast resume-condition))
      (for-each thread-join threads)
      (and (andmap (lambda (result) (eq? result #t))
                   (vector->list results))
           (andmap (lambda (task)
                     (eq? (#3%$native-fiber-state task) 'parked))
                   (vector->list tasks))))))

(mat native-fiber-switch-collector-race
  (let* ([worker-count
          (native-fiber-stress-integer
            "CHEZ_NATIVE_FIBER_RACE_WORKERS" 3 1)]
         [rounds
          (native-fiber-stress-integer
            "CHEZ_NATIVE_FIBER_RACE_ROUNDS" 500 1)]
         [m (make-mutex)]
         [done 0]
         [results (make-vector worker-count #f)])
    (define (worker i)
      (guard (condition
               [else
                (vector-set! results i condition)
                (with-mutex m (set! done (fx+ done 1)))])
        (let ([scheduler (#3%$native-fiber-adopt 0)]
              [task #f])
          (set! task
            (#3%$native-fiber-create
              (lambda ()
                (let loop ([n 0])
                  (if (fx= n rounds)
                      n
                      (let ([marker (vector i n (make-list 16 n))])
                        (#3%$native-fiber-try-claim! scheduler)
                        (let ([resume
                               (#3%$native-fiber-switch task scheduler marker)])
                          (unless (fx= resume n)
                            (error 'native-fiber-switch-collector-race
                              "unexpected resume value" resume))
                          (loop (fx+ n 1)))))))
              (native-fiber-return-handler scheduler)
              #b100))
          (#3%$native-fiber-try-claim! task)
          (let loop ([n 0]
                     [transfer
                      (#3%$native-fiber-switch scheduler task 'start)])
            (if (fx= n rounds)
                (vector-set! results i
                  (and (equal? transfer (cons 'return rounds))
                       (eq? (#3%$native-fiber-state task) 'finished)
                       (fx= (#3%$tc-field 'disable-count (#3%$tc)) 0)))
                (begin
                  (unless (and (vector? transfer)
                               (fx= (vector-ref transfer 0) i)
                               (fx= (vector-ref transfer 1) n)
                               (fx= (car (vector-ref transfer 2)) n))
                    (error 'native-fiber-switch-collector-race
                      "corrupt parked stack payload" transfer))
                  (#3%$native-fiber-try-claim! task)
                  (loop (fx+ n 1)
                    (#3%$native-fiber-switch scheduler task n)))))
          (with-mutex m (set! done (fx+ done 1))))))
    (let ([threads
           (let loop ([i 0])
             (if (fx= i worker-count)
                 '()
                 (cons (fork-thread (lambda () (worker i)))
                       (loop (fx+ i 1)))))])
      (parameterize ([#%$enable-check-heap #t])
        (let collect-loop ()
          (unless (with-mutex m (fx= done worker-count))
            (collect-rendezvous)
            (collect-loop))))
      (for-each thread-join threads)
      (andmap (lambda (result) (eq? result #t))
        (vector->list results)))))

(mat native-fiber-cross-thread-migration
  (let* ([main-scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [current-scheduler main-scheduler]
         [task #f]
         [owners '()]
         [wind-log '()]
         [root (cons 'native-fiber-migration-root (make-bytevector 4096 42))]
         [marks-ok? #t])
    (define (note-owner!)
      (set! owners (cons (get-thread-id) owners))
      (set! marks-ok?
        (and marks-ok?
             (not (continuation-marks-first
                    (current-continuation-marks)
                    'native-fiber-worker
                    #f)))))
    (define (yield payload)
      (note-owner!)
      (#3%$native-fiber-try-claim! current-scheduler)
      (let ([transfer
             (#3%$native-fiber-switch task current-scheduler payload)])
        transfer))
    (define (resume-on-new-thread payload)
      (native-fiber-resume-on-new-thread task payload
        (lambda (scheduler) (set! current-scheduler scheduler))
        (lambda (scheduler task payload)
          (with-continuation-mark 'native-fiber-worker scheduler
            (#3%$native-fiber-switch scheduler task payload)))))
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (dynamic-wind
            (lambda () (set! wind-log (cons 'in wind-log)))
            (lambda ()
              (let down ([depth 3000])
                (if (fx= depth 0)
                    (let ([multi #f])
                      (let ([phase
                             (call/cc
                               (lambda (k)
                                 (set! multi k)
                                 'initial))])
                        (case phase
                          [(initial) (yield 'first) (multi 'migrated)]
                          [(migrated) (yield 'second) (multi 'migrated-again)]
                          [(migrated-again)
                           (note-owner!)
                           (and marks-ok?
                                (eq? (car root) 'native-fiber-migration-root)
                                (fx= (bytevector-u8-ref (cdr root) 2048) 42)
                                1)]
                          [else (error 'native-fiber-cross-thread-migration
                                  "unexpected continuation phase")])))
                    (fx+ 1 (down (fx- depth 1))))))
            (lambda () (set! wind-log (cons 'out wind-log)))))
        (native-fiber-dynamic-return-handler (lambda () current-scheduler))
        #b100))
    (#3%$native-fiber-try-claim! task)
    (let ([first (#3%$native-fiber-switch main-scheduler task 'start)])
      (and (eq? first 'first)
           (not (#3%$native-fiber-owner task))
           (equal? wind-log '(in))
           (begin
             (collect (collect-maximum-generation))
             #t)
           (let ([second (resume-on-new-thread 'resume-1)])
             (and (eq? second 'second)
                  (not (#3%$native-fiber-owner task))))
           (begin
             (collect (collect-maximum-generation))
             #t)
           (let ([finished (resume-on-new-thread 'resume-2)])
             (equal? finished (cons 'return (fx+ 3000 1))))
           (eq? (#3%$native-fiber-state task) 'finished)
           marks-ok?
           (equal? wind-log '(out in))
           (fx= (length (remv (car owners) owners)) 2)
           (fx= (length owners) 3)))))

(mat native-fiber-many-thread-migration
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [current-scheduler scheduler]
         [task #f]
         [n 32])
    (define (yield value)
      (#3%$native-fiber-try-claim! current-scheduler)
      (#3%$native-fiber-switch task current-scheduler value))
    (define (resume-on-new-thread value)
      (native-fiber-resume-on-new-thread task value
        (lambda (scheduler) (set! current-scheduler scheduler))))
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (let down ([depth 2000])
            (if (fx= depth 0)
                (begin
                  (do ([i 0 (fx+ i 1)])
                      ((fx= i n))
                    (yield i))
                  n)
                (fx+ 1 (down (fx- depth 1))))))
        (native-fiber-dynamic-return-handler (lambda () current-scheduler))
        #b100))
    (#3%$native-fiber-try-claim! task)
    (let ([first (#3%$native-fiber-switch scheduler task 'start)])
      (and (fx= first 0)
           (let loop ([i 1])
             (cond
               [(fx= i n)
                (let ([finished (resume-on-new-thread 'resume)])
                  (and (equal? finished
                         (cons 'return (fx+ 2000 n)))
                       (eq? (#3%$native-fiber-state task) 'finished)))]
               [else
                (when (fx= (fxlogand i 7) 0)
                  (collect (collect-maximum-generation)))
                (let ([yielded (resume-on-new-thread 'resume)])
                  (and (fx= yielded i)
                       (not (#3%$native-fiber-owner task))
                       (loop (fx+ i 1))))]))))))

(mat native-fiber-claim-race
  (let* ([main-scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [current-scheduler main-scheduler]
         [task #f]
         [n 8]
         [m (make-mutex)]
         [c (make-condition)]
         [ready 0]
         [go? #f]
         [won (make-vector n #f)]
         [valid (make-vector n #t)])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (#3%$native-fiber-try-claim! current-scheduler)
          (#3%$native-fiber-switch task current-scheduler 'parked)
          'winner)
        (native-fiber-dynamic-return-handler (lambda () current-scheduler))
        #b100))
    (#3%$native-fiber-try-claim! task)
    (let ([parked (#3%$native-fiber-switch main-scheduler task 'start)])
      (and (eq? parked 'parked)
           (let ([threads
                  (let loop ([i 0])
                    (if (fx= i n)
                        '()
                        (cons
                          (fork-thread
                            (lambda ()
                              (let ([scheduler (#3%$native-fiber-adopt 0)])
                                (with-mutex m
                                  (set! ready (fx+ ready 1))
                                  (condition-broadcast c)
                                  (let wait ()
                                    (unless go?
                                      (condition-wait c m)
                                      (wait))))
                                (when (#3%$native-fiber-try-claim! task)
                                  (set! current-scheduler scheduler)
                                  (vector-set! won i #t)
                                  (let ([finished
                                         (#3%$native-fiber-switch
                                           scheduler task 'resume)])
                                    (vector-set! valid i
                                      (and (equal? finished
                                             '(return . winner)))))))))
                          (loop (fx+ i 1)))))])
             (with-mutex m
               (let wait ()
                 (unless (fx= ready n)
                   (condition-wait c m)
                   (wait)))
               (set! go? #t)
               (condition-broadcast c))
             (for-each thread-join threads)
             (and (fx= (length (filter values (vector->list won))) 1)
                  (andmap values (vector->list valid))
                  (eq? (#3%$native-fiber-state task) 'finished)))))))

(mat native-fiber-publication-litmus
  (let* ([main-scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [current-scheduler main-scheduler]
         [task #f]
         [publication #f]
         [rounds
          (native-fiber-stress-integer
            "CHEZ_NATIVE_FIBER_PUBLICATION_ROUNDS" 2000 1)]
         [worker-count
          (native-fiber-stress-integer
            "CHEZ_NATIVE_FIBER_PUBLICATION_WORKERS" 4 2)]
         [failure #f]
         [owners '()]
         [lock (make-mutex)]
         [contention-pause (make-time 'time-duration 100000 0)])
    (define (make-publication i)
      (vector i (make-vector 64 i) (fxlogxor i #x5a5a5a)))
    (define (valid-publication? value)
      (and (vector? value)
           (fx= (vector-length value) 3)
           (let ([i (vector-ref value 0)]
                 [body (vector-ref value 1)])
             (and (fixnum? i)
                  (vector? body)
                  (fx= (vector-length body) 64)
                  (let loop ([j 0])
                    (or (fx= j 64)
                        (and (fx= (vector-ref body j) i)
                             (loop (fx+ j 1)))))
                  (fx= (vector-ref value 2) (fxlogxor i #x5a5a5a))))))
    (define (yield-publication i)
      (set! publication (make-publication i))
      (#3%$native-fiber-try-claim! current-scheduler)
      (let ([ack (#3%$native-fiber-switch
                   task current-scheduler publication)])
        (unless (fx= ack i)
          (error 'native-fiber-publication-litmus
            "resume acknowledgement was not published" i ack))))
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (do ([i 0 (fx+ i 1)])
              ((fx= i rounds) 'done)
            (yield-publication i)))
        (native-fiber-dynamic-return-handler (lambda () current-scheduler))
        #b100))
    (#3%$native-fiber-try-claim! task)
    (let ([first (#3%$native-fiber-switch main-scheduler task 'start)])
      (unless (and (eq? first publication) (valid-publication? first))
        (set! failure 'initial-publication)))
    (let ([workers
           (map
             (lambda (worker-index)
               (fork-thread
                 (lambda ()
                   (let ([scheduler (#3%$native-fiber-adopt 0)])
                     (let loop ()
                       (let ([claim
                              (guard (condition
                                       [else
                                        (with-mutex lock
                                          (unless failure
                                            (set! failure
                                              (list 'claim-exception
                                                worker-index condition))))
                                        'failed])
                                (if (#3%$native-fiber-try-claim! task)
                                    'won
                                    'lost))])
                         (cond
                           [(eq? claim 'failed) (void)]
                           [(eq? claim 'won)
                            ;; The successful claim's acquire is the only
                            ;; synchronization before these ordinary reads.
                            (let ([seen publication])
                              (unless (valid-publication? seen)
                                (with-mutex lock
                                  (unless failure
                                    (set! failure
                                      (list 'torn-publication worker-index)))))
                              (set! current-scheduler scheduler)
                              (let ([outcome
                                     (#3%$native-fiber-switch scheduler task
                                       (if (valid-publication? seen)
                                           (vector-ref seen 0)
                                           -1))])
                                (with-mutex lock
                                  (set! owners
                                    (cons (get-thread-id) owners)))
                                (unless (or (and (pair? outcome)
                                                 (eq? (car outcome) 'return))
                                            (valid-publication? outcome))
                                  (with-mutex lock
                                    (unless failure
                                      (set! failure
                                        (list 'payload-publication
                                              worker-index)))))
                                ;; Give another thief a chance to perform the
                                ;; next acquire instead of repeatedly winning on
                                ;; one native worker.
                                (sleep contention-pause)
                                (loop)))]
                           [(eq? (#3%$native-fiber-state task) 'finished)
                            (void)]
                           [else
                            (sleep contention-pause)
                            (loop)])))))))
             (iota worker-count))])
      (for-each thread-join workers)
      (let ([owner-count
             (let unique ([owner* owners] [seen '()])
               (if (null? owner*)
                   (length seen)
                   (unique (cdr owner*)
                     (if (memv (car owner*) seen)
                         seen
                         (cons (car owner*) seen)))))])
        (unless (and (not failure)
                     (eq? (#3%$native-fiber-state task) 'finished)
                     (fx= (vector-ref publication 0) (fx- rounds 1))
                     ;; Multiple workers must participate, otherwise the loop
                     ;; does not exercise cross-thread publication.
                     (fx> owner-count 1))
          (error 'native-fiber-publication-litmus
            "publication stress failed"
            failure
            (#3%$native-fiber-state task)
            (vector-ref publication 0)
            owner-count))
        #t))))

(define (native-fiber-release-claim-litmus-once)
  (let* ([task
          (#3%$native-fiber-create
            (lambda () 'unused)
            (lambda args (error 'native-fiber-release-claim-litmus
                           "unreachable termination"))
            #b100)]
         [lock (make-mutex)]
         [condition (make-condition)]
         [first-claimed? #f]
         [publication #f]
         [observed #f])
    (let ([first
           (fork-thread
             (lambda ()
               (#3%$native-fiber-adopt 0)
               (unless (#3%$native-fiber-try-claim! task)
                 (error 'native-fiber-release-claim-litmus
                   "first claim failed"))
               (with-mutex lock
                 (set! first-claimed? #t)
                 (condition-broadcast condition))
               ;; This write is after the mutex publication. The second
               ;; worker can observe it only through release-claim/claim.
               (set! publication
                 (vector #x12345678 (make-vector 64 'published)))
               (#3%$native-fiber-release-claim! task)))]
          [second
           (fork-thread
             (lambda ()
               (#3%$native-fiber-adopt 0)
               (with-mutex lock
                 (let wait ()
                   (unless first-claimed?
                     (condition-wait condition lock)
                     (wait))))
               (let claim ()
                 (unless (#3%$native-fiber-try-claim! task)
                   (sleep (make-time 'time-duration 1000 0))
                   (claim)))
               (set! observed
                 (and (vector? publication)
                      (fx= (vector-ref publication 0) #x12345678)
                      (andmap (lambda (x) (eq? x 'published))
                        (vector->list (vector-ref publication 1)))))
               (#3%$native-fiber-release-claim! task)))])
      (thread-join first)
      (thread-join second)
      (and observed (eq? (#3%$native-fiber-state task) 'new)))))

(mat native-fiber-release-claim-litmus
  (let ([rounds
         (native-fiber-stress-integer
           "CHEZ_NATIVE_FIBER_CLAIM_PUBLICATION_ROUNDS" 64 1)])
    (let loop ([round 0])
      (or (fx= round rounds)
          (and (native-fiber-release-claim-litmus-once)
               (loop (fx+ round 1)))))))

(mat native-fiber-pinned-owner
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [task #f])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (#3%$native-fiber-try-claim! scheduler)
          (#3%$native-fiber-switch task scheduler 'parked)
          'done)
        (native-fiber-return-handler scheduler)
        #b001))
    (#3%$native-fiber-try-claim! task)
    (let ([first (#3%$native-fiber-switch scheduler task 'start)]
          [foreign-claim #t])
      (thread-join
        (fork-thread
          (lambda ()
            (#3%$native-fiber-adopt 0)
            (set! foreign-claim (#3%$native-fiber-try-claim! task)))))
      (and (eq? first 'parked)
           (not foreign-claim)
           (fx= (#3%$native-fiber-owner task) (get-thread-id))
           (#3%$native-fiber-try-claim! task)
           (let ([finished (#3%$native-fiber-switch scheduler task 'resume)])
             (equal? finished '(return . done)))
           (eq? (#3%$native-fiber-state task) 'finished)))))

(mat native-fiber-pin-transition
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [current-scheduler scheduler]
         [task #f]
         [foreign-claim #t]
         [duplicate-pin-error? #f]
         [duplicate-unpin-error? #f])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (#3%$native-fiber-pin! task)
          (set! duplicate-pin-error?
            (guard (c [else #t])
              (#3%$native-fiber-pin! task)
              #f))
          (#3%$native-fiber-try-claim! current-scheduler)
          (#3%$native-fiber-switch task current-scheduler 'pinned)
          (#3%$native-fiber-unpin! task)
          (set! duplicate-unpin-error?
            (guard (c [else #t])
              (#3%$native-fiber-unpin! task)
              #f))
          (#3%$native-fiber-try-claim! current-scheduler)
          (#3%$native-fiber-switch task current-scheduler 'migratable)
          'done)
        (native-fiber-dynamic-return-handler (lambda () current-scheduler))
        #b100))
    (#3%$native-fiber-try-claim! task)
    (let ([pinned (#3%$native-fiber-switch scheduler task 'start)])
      (thread-join
        (fork-thread
          (lambda ()
            (#3%$native-fiber-adopt 0)
            (set! foreign-claim (#3%$native-fiber-try-claim! task)))))
      (and (eq? pinned 'pinned)
           (not foreign-claim)
           duplicate-pin-error?
           (fx= (#3%$native-fiber-flags task) #b001)
           (#3%$native-fiber-try-claim! task)
           (let ([migratable
                  (#3%$native-fiber-switch scheduler task 'resume)])
             (eq? migratable 'migratable))
           (not (#3%$native-fiber-owner task))
           duplicate-unpin-error?
           (fx= (#3%$native-fiber-flags task) #b100)
           (let ([finished #f])
             (thread-join
               (fork-thread
                 (lambda ()
                   (let ([worker (#3%$native-fiber-adopt 0)])
                     (set! current-scheduler worker)
                     (set! finished
                       (and (#3%$native-fiber-try-claim! task)
                            (#3%$native-fiber-switch worker task 'resume)))))))
             (equal? finished '(return . done)))
           (eq? (#3%$native-fiber-state task) 'finished)))))

(mat native-fiber-dead-stack-slots
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [task #f]
         [running-weak #f]
         [parked-weak #f])
    (define (check-running-dead-slot)
      (let ([dead (vector 'running-dead-slot
                          (make-bytevector 8192 17))])
        (set! running-weak (weak-cons dead #f))
        (unless (eq? (vector-ref dead 0) 'running-dead-slot)
          (error 'native-fiber-dead-stack-slots "lost running root")))
      (parameterize ([#%$enable-check-heap #t])
        (collect (collect-maximum-generation))))
    (check-running-dead-slot)
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (let ([dead (vector 'parked-dead-slot
                              (make-bytevector 8192 23))])
            (set! parked-weak (weak-cons dead #f))
            (unless (eq? (vector-ref dead 0) 'parked-dead-slot)
              (error 'native-fiber-dead-stack-slots "lost parked root")))
          (#3%$native-fiber-try-claim! scheduler)
          (#3%$native-fiber-switch task scheduler 'parked)
          'done)
        (native-fiber-return-handler scheduler)
        #b100))
    (#3%$native-fiber-try-claim! task)
    (and (eq? (#3%$native-fiber-switch scheduler task 'start) 'parked)
         (begin
           (parameterize ([#%$enable-check-heap #t])
             (collect (collect-maximum-generation)))
           #t)
         (bwp-object? (car running-weak))
         (bwp-object? (car parked-weak))
         (#3%$native-fiber-try-claim! task)
         (equal? (#3%$native-fiber-switch scheduler task 'resume)
                 '(return . done))
         (eq? (#3%$native-fiber-state task) 'finished))))

(mat compiler-dead-location-clobbering
  (let ([run
         (parameterize ([#3%$compile-clobber-dead
                         (or (#3%$compile-clobber-dead) 424242)]
                        [#3%$compile-clobber-dead-registers
                         (or (#3%$compile-clobber-dead-registers) 171717)])
           (eval
             '(lambda (limit invoke)
                (let loop ([i 0] [sum 0])
                  (if (fx= i limit)
                      sum
                      (loop (fx+ i 1)
                        (fx+ sum (invoke i))))))
             (interaction-environment)))])
    (fx= (run 100 (lambda (i) (fx+ i 1))) 5050)))

(mat native-fiber-stack-map-matrix
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [task #f]
         [current-weaks #f]
         [current-live #f]
         [widths '(3 17 31 32 33 63 64 65 95 96 127 128 159)]
         [shapes
          (parameterize ([#3%$compile-clobber-dead
                          (or (#3%$compile-clobber-dead) 8675309)]
                         [#3%$compile-clobber-dead-registers
                          (or (#3%$compile-clobber-dead-registers) 314159)])
            (map
              (lambda (width)
                (let* ([name* (map (lambda (i)
                                   (string->symbol (format "stack-map-~s" i)))
                                 (iota width))]
                     [live?* (map (lambda (i)
                                    (or (fx= i (fx- width 1))
                                        (fx= (fxmod (fx+ (fx* i 17) width) 5)
                                             0)))
                                  (iota width))]
                     [binding*
                      (map (lambda (name i)
                             `(,name (vector ',name
                                             (make-bytevector 96
                                               (fxlogand ,i 255)))))
                           name* (iota width))]
                     [live-name*
                      (let loop ([name* name*] [live?* live?*] [out '()])
                        (if (null? name*)
                            (reverse out)
                            (loop (cdr name*) (cdr live?*)
                              (if (car live?*)
                                  (cons (car name*) out)
                                  out))))])
                  (eval
                    `(lambda (suspend publish)
                       (let* ,binding*
                         (publish
                           (vector ,@(map (lambda (name)
                                           `(weak-cons ,name #f))
                                         name*))
                           ',(list->vector live?*))
                         (let ([resume (suspend ,width)])
                           (and (fx= resume ,width)
                                (list ,@live-name*)))))
                    (interaction-environment))))
              widths))])
    (define (publish weak* live*)
      (set! current-weaks weak*)
      (set! current-live live*))
    (define (yield value)
      (#3%$native-fiber-try-claim! scheduler)
      (#3%$native-fiber-switch task scheduler value))
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (let loop ([shape* shapes] [width* widths])
            (if (null? shape*)
                'done
                (let ([result ((car shape*) yield publish)])
                  (unless (and result
                               (fx= (length result)
                                    (let count ([i 0] [n 0])
                                      (if (fx= i (vector-length current-live))
                                          n
                                          (count (fx+ i 1)
                                            (if (vector-ref current-live i)
                                                (fx+ n 1)
                                                n))))))
                    (error 'native-fiber-stack-map-matrix
                      "live stack roots were corrupted" (car width*)))
                  (loop (cdr shape*) (cdr width*))))))
        (native-fiber-return-handler scheduler)
        #b100))
    (#3%$native-fiber-try-claim! task)
    (let loop ([width* widths]
               [transfer (#3%$native-fiber-switch scheduler task 'start)])
      (if (null? width*)
          (and (equal? transfer '(return . done))
               (eq? (#3%$native-fiber-state task) 'finished))
          (and (fx= transfer (car width*))
               (begin
                 (parameterize ([#%$enable-check-heap #t])
                   (collect (collect-maximum-generation)))
                 #t)
               (let check ([i 0])
                 (or (fx= i (vector-length current-live))
                     (and (if (vector-ref current-live i)
                              (not (bwp-object?
                                     (car (vector-ref current-weaks i))))
                              (bwp-object?
                                (car (vector-ref current-weaks i))))
                          (check (fx+ i 1)))))
               (#3%$native-fiber-try-claim! task)
               (loop (cdr width*)
                 (#3%$native-fiber-switch scheduler task (car width*))))))))

(mat native-fiber-stack-shape-matrix
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [current-scheduler scheduler]
         [task #f]
         [wide-names
          (let loop ([i 0])
            (if (fx= i 96)
                '()
                (cons (string->symbol (format "wide-~s" i))
                      (loop (fx+ i 1)))))]
         [wide-roots
          (map (lambda (i) (vector 'wide-root i (make-bytevector 64 i)))
               (iota 96))]
         [wide
          (eval `(lambda (suspend ,@wide-names)
                   (let ([resume (suspend 'wide)])
                     (cons resume (list ,@wide-names)))))]
         [wind-in 0]
         [wind-out 0]
         [mark-count 0]
         [exception-text #f])
    (define (yield value)
      (#3%$native-fiber-try-claim! current-scheduler)
      (#3%$native-fiber-switch task current-scheduler value))
    (define (wind depth)
      (with-continuation-mark 'native-fiber-stack-shape depth
        (dynamic-wind
          (lambda () (set! wind-in (fx+ wind-in 1)))
          (lambda ()
            (if (fx= depth 0)
                (begin
                  (set! mark-count
                    (length
                      (continuation-marks->list
                        (current-continuation-marks)
                        'native-fiber-stack-shape)))
                  (yield 'wind))
                (fx+ 1 (wind (fx- depth 1)))))
          (lambda () (set! wind-out (fx+ wind-out 1))))))
    (define (deep round depth)
      (let ([root (vector round depth
                          (make-bytevector 32 (fxlogand depth 255)))])
        (if (fx= depth 0)
            (yield (cons 'deep round))
            (fx+ (vector-ref root 1) (deep round (fx- depth 1))))))
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (let ([wide-result (apply wide yield wide-roots)])
            (unless (and (fx= (car wide-result) 7)
                         (andmap eq? (cdr wide-result) wide-roots))
              (error 'native-fiber-stack-shape-matrix
                "non-compact frame roots were corrupted")))
          (wind 192)
          (do ([round 0 (fx+ round 1)])
              ((fx= round 4))
            (unless (fx= (deep round 6000)
                         (fx+ 7 (fxquotient (fx* 6000 6001) 2)))
              (error 'native-fiber-stack-shape-matrix
                "deep stack result was corrupted" round)))
          (guard (condition
                   [else
                    (set! exception-text
                      (with-output-to-string
                        (lambda () (display-condition condition))))])
            (error 'native-fiber-stack-shape-matrix
              "stack-shape-backtrace"))
          (list wind-in wind-out mark-count exception-text))
        (native-fiber-dynamic-return-handler (lambda () current-scheduler))
        #b100))
    (#3%$native-fiber-try-claim! task)
    (let loop ([expected '(wide wind (deep . 0) (deep . 1)
                                      (deep . 2) (deep . 3))]
               [transfer
                (#3%$native-fiber-switch scheduler task 'start)])
      (cond
        [(pair? expected)
         (and (equal? transfer (car expected))
              (begin
                (parameterize ([#%$enable-check-heap #t])
                  (collect (collect-maximum-generation)))
                #t)
              (#3%$native-fiber-try-claim! task)
              (loop (cdr expected)
                (#3%$native-fiber-switch scheduler task 7)))]
        [else
         (and (pair? transfer)
              (eq? (car transfer) 'return)
              (let ([result (cdr transfer)])
                (and (fx= (list-ref result 0) 193)
                     (fx= (list-ref result 1) 193)
                     (fx= (list-ref result 2) 193)
                     (string? (list-ref result 3))
                     (native-fiber-string-contains?
                       (list-ref result 3) "stack-shape-backtrace")))
              (eq? (#3%$native-fiber-state task) 'finished))]))))

(mat native-fiber-foreign-callback-gc
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [call-callback
          (foreign-procedure "call_for_interrupt_test" (void* int) int)]
         [task #f]
         [callback-depth #f]
         [root-survived? #f])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (let* ([root (vector 'foreign-callback-root
                               (make-bytevector 16384 91))]
                 [callback
                  (foreign-callable
                    (lambda (value)
                      (set! callback-depth
                        (#3%$fiber-switch-prohibited-depth))
                      (parameterize ([#%$enable-check-heap #t])
                        (collect (collect-maximum-generation)))
                      (set! root-survived?
                        (and (eq? (vector-ref root 0)
                                  'foreign-callback-root)
                             (fx= (bytevector-u8-ref (vector-ref root 1) 8192)
                                  91)))
                      (fx+ value 1))
                    (int) int)]
                 [result
                  (call-callback
                    (foreign-callable-entry-point callback)
                    41)])
            (keep-live callback)
            (#3%$native-fiber-try-claim! scheduler)
            (#3%$native-fiber-switch task scheduler result)
            'done))
        (native-fiber-return-handler scheduler)
        #b001))
    (#3%$native-fiber-try-claim! task)
    (and (fx= (#3%$native-fiber-switch scheduler task 'start) 42)
         (fx= callback-depth 1)
         root-survived?
         (#3%$native-fiber-try-claim! task)
         (equal? (#3%$native-fiber-switch scheduler task 'resume)
                 '(return . done))
         (eq? (#3%$native-fiber-state task) 'finished))))

(mat native-fiber-collect-safe-blocking-gc
  (begin
    (load-thread-foreign-library!)
    (let* ([main-scheduler
            (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
           [current-scheduler main-scheduler]
           [block-reset
            (foreign-procedure "collect_safe_block_reset" () void)]
           [block-state
            (foreign-procedure "collect_safe_block_state" () int)]
           [block-release
            (foreign-procedure "collect_safe_block_release" () void)]
           [block
            (foreign-procedure __collect_safe
              "collect_safe_block_until_release" (int) int)]
           [task #f]
           [collector-finished? #f]
           [root-valid-after-call? #f]
           [root-valid-after-migration? #f])
      (define (root-valid? root)
        (and (eq? (vector-ref root 0) 'collect-safe-root)
             (fx= (bytevector-u8-ref (vector-ref root 1) 8192) 83)))
      (define (resume-on-worker payload)
        (native-fiber-resume-on-new-thread task payload
          (lambda (scheduler) (set! current-scheduler scheduler))))
      (block-reset)
      (set! task
        (#3%$native-fiber-create
          (lambda ()
            (let* ([root
                    (vector 'collect-safe-root
                      (make-bytevector 16384 83))]
                   [released-state (block 10000)])
              (set! root-valid-after-call? (root-valid? root))
              (#3%$native-fiber-try-claim! current-scheduler)
              (let ([resume
                     (#3%$native-fiber-switch
                       task current-scheduler released-state)])
                (set! root-valid-after-migration? (root-valid? root))
                resume)))
          (native-fiber-dynamic-return-handler (lambda () current-scheduler))
          #b100))
      (let ([collector
             (fork-thread
               (lambda ()
                 (let wait ()
                   (case (block-state)
                     [(0)
                      (sleep (make-time 'time-duration 1000 0))
                      (wait)]
                     [(1)
                      (parameterize ([#%$enable-check-heap #t])
                        (collect (collect-maximum-generation)))
                      (set! collector-finished? #t)
                      (block-release)]
                     [else (void)]))))])
        (#3%$native-fiber-try-claim! task)
        (let ([released-state
               (#3%$native-fiber-switch main-scheduler task 'start)])
          (thread-join collector)
          (and (fx= released-state 2)
               collector-finished?
               root-valid-after-call?
               (equal? (resume-on-worker 'collect-safe-resumed)
                       '(return . collect-safe-resumed))
               root-valid-after-migration?
               (eq? (#3%$native-fiber-state task) 'finished)))))))

(if (windows?)
    (mat native-fiber-nested-signal-altstack #t)
    (mat native-fiber-nested-signal-altstack
      (begin
        (load-thread-foreign-library!)
        (let* ([scheduler
                (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
               [run-altstack
                (foreign-procedure "run_signal_altstack_test"
                  (int boolean) int)]
               [task #f]
               [root #f]
               [deliveries 0]
               [relay-deliveries #f]
               [handler-fiber-valid? #t]
               [handler-root-valid? #t])
          (set! task
            (#3%$native-fiber-create
              (lambda ()
                (set! root
                  (vector 'altstack-signal-root
                    (make-bytevector 16384 97)))
                (set! relay-deliveries (run-altstack 14 #t))
                (let wait ([fuel 10000000])
                  (unless (fx= deliveries 2)
                    (when (fx= fuel 0)
                      (error 'native-fiber-nested-signal-altstack
                        "nested signals were not delivered"
                        relay-deliveries deliveries))
                    (vector fuel root)
                    (wait (fx- fuel 1))))
                'done)
              (native-fiber-return-handler scheduler)
              #b001))
          (dynamic-wind
            (lambda ()
              (register-signal-handler
                14
                (lambda (signal)
                  (unless (fx= signal 14)
                    (set! handler-fiber-valid? #f))
                  (set! deliveries (fx+ deliveries 1))
                  (unless (eq? (#3%$current-native-fiber) task)
                    (set! handler-fiber-valid? #f))
                  (unless (and (eq? (vector-ref root 0)
                                    'altstack-signal-root)
                               (fx= (bytevector-u8-ref
                                      (vector-ref root 1) 8192)
                                    97))
                    (set! handler-root-valid? #f)))))
            (lambda ()
              (#3%$native-fiber-try-claim! task)
              (let ([outcome (#3%$native-fiber-switch scheduler task 'start)])
                (and (equal? outcome '(return . done))
                     (fx= relay-deliveries 2)
                     (fx= deliveries 2)
                     handler-fiber-valid?
                     handler-root-valid?
                     (eq? (#3%$native-fiber-state task) 'finished))))
            (lambda ()
              (register-signal-handler 14 void)))))))

(if (windows?)
    (mat native-fiber-signal-preemption-gc #t)
    (mat native-fiber-signal-preemption-gc
      (let* ([scheduler
              (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
             [task #f]
             [root #f]
             [signal-seen? #f]
             [signal-fiber-ok? #f]
             [signal-root-ok? #f]
             [signal-gc-finished? #f]
             [preempt-requested? #f]
             [resumed? #f]
             [saved-handler (timer-interrupt-handler)])
        (set! task
          (#3%$native-fiber-create
            (lambda ()
              (set! root
                (vector 'signal-preemption-root
                        (make-bytevector 16384 77)))
              (set-timer 1000)
              (guard (condition
                       [else
                        (display-condition condition)
                        (newline)])
                (system "exec kill -14 $PPID"))
              (let wait-signal ([fuel 10000000])
                (unless signal-seen?
                  (when (fx= fuel 0)
                    (error 'native-fiber-signal-preemption-gc
                      "signal was not delivered"))
                  (vector fuel)
                  (wait-signal (fx- fuel 1))))
              (let wait-preemption ([fuel 10000000])
                (unless resumed?
                  (when (fx= fuel 0)
                    (error 'native-fiber-signal-preemption-gc
                      "timer preemption was not delivered"))
                  (vector fuel root)
                  (wait-preemption (fx- fuel 1))))
              'completed)
            (native-fiber-return-handler scheduler)
            #b001))
        (dynamic-wind
          (lambda ()
            (register-signal-handler
              14
              (lambda (signal)
                (set! signal-seen? (fx= signal 14))
                (set! signal-fiber-ok?
                  (eq? (#3%$current-native-fiber) task))
                (parameterize ([#%$enable-check-heap #t])
                  (collect (collect-maximum-generation)))
                (set! signal-root-ok?
                  (and (eq? (vector-ref root 0)
                            'signal-preemption-root)
                       (fx= (bytevector-u8-ref (vector-ref root 1) 8192)
                            77)))
                (set! signal-gc-finished? #t)))
            (timer-interrupt-handler
              (lambda ()
                (if signal-gc-finished?
                    (set! preempt-requested?
                      (#3%$native-fiber-preempt
                        scheduler 'signal-preempted))
                    (set-timer 1)))))
          (lambda ()
            (#3%$native-fiber-try-claim! task)
            (and (eq? (#3%$native-fiber-switch scheduler task 'start)
                      'signal-preempted)
                 signal-seen?
                 signal-fiber-ok?
                 signal-root-ok?
                 signal-gc-finished?
                 preempt-requested?
                 (begin (set! resumed? #t) #t)
                 (#3%$native-fiber-try-claim! task)
                 (equal? (#3%$native-fiber-switch scheduler task 'resume)
                         '(return . completed))
                 (eq? (#3%$native-fiber-state task) 'finished)))
          (lambda ()
            (set-timer 0)
            (timer-interrupt-handler saved-handler)
            (register-signal-handler 14 void))))))

(mat native-fiber-foreign-callback-boundary
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [call-callback
          (foreign-procedure "call_for_interrupt_test" (void* int) int)]
         [task #f]
         [blocked? #f]
         [callback-depth #f]
         [nested-callback-depth #f]
         [after-depth #f])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (let* ([inner-callback
                  (foreign-callable
                    (lambda (value)
                      (set! nested-callback-depth
                        (#3%$fiber-switch-prohibited-depth))
                      (fx+ value 1))
                    (int) int)]
                 [callback
                  (foreign-callable
                    (lambda (value)
                      (set! callback-depth
                        (#3%$fiber-switch-prohibited-depth))
                      (guard (condition
                               [else
                                (set! blocked? #t)])
                        (#3%$native-fiber-try-claim! scheduler)
                        (#3%$native-fiber-switch task scheduler 'escaped)
                        (void))
                      (fx+ (call-callback
                             (foreign-callable-entry-point inner-callback)
                             value)
                           1))
                    (int) int)]
                 [result
                  (call-callback
                    (foreign-callable-entry-point callback)
                    40)])
            (keep-live callback)
            (keep-live inner-callback)
            (set! after-depth (#3%$fiber-switch-prohibited-depth))
            (#3%$native-fiber-switch task scheduler result)
            'done))
        (native-fiber-return-handler scheduler)
        #b001))
    (#3%$native-fiber-try-claim! task)
    (let ([yielded (#3%$native-fiber-switch scheduler task 'start)])
      (and (fx= yielded 42)
           blocked?
           (fx= callback-depth 1)
           (fx= nested-callback-depth 2)
           (fx= after-depth 0)
           (eq? (#3%$native-fiber-state task) 'parked)
           (#3%$native-fiber-try-claim! task)
           (let ([finished (#3%$native-fiber-switch scheduler task 'resume)])
             (equal? finished '(return . done)))
           (eq? (#3%$native-fiber-state task) 'finished)))))

(mat native-fiber-foreign-abi-migration
  (begin
    (load-thread-foreign-library!)
    (let* ([main-scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [current-scheduler main-scheduler]
         [call-i64
          (foreign-procedure
            "call_i64" (ptr integer-64 int int) integer-64)]
         [call-df
          (foreign-procedure
            "call_df" (ptr double-float int int) double-float)]
         [call-many
          (foreign-procedure "call_with_many_args" (void*) void)]
         [task #f]
         [callback-depths '()]
         [many-ok? #t])
    (define (note-depth!)
      (set! callback-depths
        (cons (#3%$fiber-switch-prohibited-depth) callback-depths)))
    (define (yield value)
      (#3%$native-fiber-try-claim! current-scheduler)
      (#3%$native-fiber-switch task current-scheduler value))
    (define (resume-on-worker value)
      (native-fiber-resume-on-new-thread task value
        (lambda (scheduler) (set! current-scheduler scheduler))))
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (let ([i64-callback
                 (foreign-callable
                   (lambda (x)
                     (note-depth!)
                     (collect (collect-maximum-generation))
                     (+ x 5))
                   (integer-64) integer-64)]
                [df-callback
                 (foreign-callable
                   (lambda (x)
                     (note-depth!)
                     (+ x 5.0))
                   (double-float) double-float)]
                [many-callback
                 (foreign-callable
                   (lambda (i s1 s2 s3 s4 i2 s6 s7 i3)
                     (note-depth!)
                     (set! many-ok?
                       (and many-ok?
                            (fx= i 0)
                            (equal? (string->utf8 "this") s1)
                            (equal? (string->utf8 "is") s2)
                            (equal? (string->utf8 "working") s3)
                            (equal? (string->utf8 "just") s4)
                            (fx= i2 1)
                            (equal? (string->utf8 "fine") s6)
                            (equal? (string->utf8 "or does it?") s7)
                            (fx= i3 2))))
                   (int u8* u8* u8* u8* int u8* u8* int)
                   void)])
            (define (check-abi)
              (and (= (call-i64 i64-callback 1099511627776 7 23)
                      1099511627811)
                   (= (call-df df-callback 73.25 7 23) 108.25)
                   (begin
                     (call-many (foreign-callable-entry-point many-callback))
                     many-ok?)))
            (and (check-abi)
                 (eq? (yield 'ffi-parked) 'ffi-resume)
                 (check-abi)
                 (begin
                   (keep-live i64-callback)
                   (keep-live df-callback)
                   (keep-live many-callback)
                   #t))))
        (native-fiber-dynamic-return-handler (lambda () current-scheduler))
        #b100))
    (#3%$native-fiber-try-claim! task)
    (and (eq? (#3%$native-fiber-switch main-scheduler task 'start)
              'ffi-parked)
         (not (#3%$native-fiber-owner task))
         (equal? (resume-on-worker 'ffi-resume) '(return . #t))
         many-ok?
         (andmap (lambda (depth) (fx= depth 1)) callback-depths)
         (fx= (length callback-depths) 6)
         (eq? (#3%$native-fiber-state task) 'finished)))))

(mat native-fiber-timer-foreign-callback-boundary
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [call-callback
          (foreign-procedure "call_for_interrupt_test" (void* int) int)]
         [task #f]
         [callback-timer-seen? #f]
         [callback-preempt-rejected? #f]
         [outside-preempted? #f]
         [saved-handler (timer-interrupt-handler)])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (let ([callback
                 (foreign-callable
                   (lambda (value)
                     (set-timer 1)
                     (let loop ([fuel 10000000])
                       (unless callback-timer-seen?
                         (when (fx= fuel 0)
                           (error 'native-fiber-timer-foreign-callback-boundary
                             "timer was not delivered in callback"))
                         (vector fuel)
                         (loop (fx- fuel 1))))
                     (fx+ value 1))
                   (int) int)])
            (unless (fx= (call-callback
                           (foreign-callable-entry-point callback) 40)
                         41)
              (error 'native-fiber-timer-foreign-callback-boundary
                "foreign callback result was corrupted"))
            (set-timer 1)
            (let loop ([fuel 10000000])
              (unless outside-preempted?
                (when (fx= fuel 0)
                  (error 'native-fiber-timer-foreign-callback-boundary
                    "timer did not preempt after callback"))
                (vector fuel)
                (loop (fx- fuel 1))))
            (keep-live callback)
            'done))
        (native-fiber-return-handler scheduler)
        #b001))
    (dynamic-wind
      (lambda ()
        (timer-interrupt-handler
          (lambda ()
            (if (fx> (#3%$fiber-switch-prohibited-depth) 0)
                (begin
                  (set! callback-timer-seen? #t)
                  (set! callback-preempt-rejected?
                    (not (#3%$native-fiber-preempt
                           scheduler 'callback-preempted))))
                (set! outside-preempted?
                  (#3%$native-fiber-preempt scheduler 'outside-preempted))))))
      (lambda ()
        (#3%$native-fiber-try-claim! task)
        (and (eq? (#3%$native-fiber-switch scheduler task 'start)
                  'outside-preempted)
             callback-timer-seen?
             callback-preempt-rejected?
             outside-preempted?
             (#3%$native-fiber-try-claim! task)
             (equal? (#3%$native-fiber-switch scheduler task 'resume)
                     '(return . done))
             (eq? (#3%$native-fiber-state task) 'finished)))
      (lambda ()
        (set-timer 0)
        (timer-interrupt-handler saved-handler)))))

(mat native-fiber-adopt-foreign-callback-boundary
  (let ([result #f])
    (let ([thread
           (fork-thread
             (lambda ()
               (let* ([call-callback
                       (foreign-procedure
                         "call_for_interrupt_test" (void* int) int)]
                      [callback
                       (foreign-callable
                         (lambda (value)
                           (let ([depth
                                  (#3%$fiber-switch-prohibited-depth)])
                             (guard (condition
                                      [else
                                       (set! result
                                         (and (fx= depth 1)
                                              (not (#3%$current-native-fiber))))
                                       value])
                               (#3%$native-fiber-adopt 0)
                               (set! result #f)
                               value)))
                         (int) int)])
                 (call-callback
                   (foreign-callable-entry-point callback)
                   37)
                 (keep-live callback))))])
      (thread-join thread)
      result)))

(mat native-fiber-engine-boundary
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [task #f]
         [task-blocked? #f]
         [engine-blocked? #f]
         [adopt-blocked? #f])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (set! task-blocked?
            (guard (condition [else #t])
              (let ([engine (make-engine (lambda () 'escaped))])
                (engine 10000 (lambda args #f) (lambda args #f)))
              #f))
          'done)
        (native-fiber-return-handler scheduler)
        #b001))
    (let ([engine
           (make-engine
             (lambda ()
               (#3%$native-fiber-try-claim! task)
               (set! engine-blocked?
                 (guard (condition [else #t])
                   (#3%$native-fiber-switch scheduler task 'escaped)
                   #f))))])
      (engine 10000 (lambda args (void)) (lambda args (void))))
    (let ([worker-result #f])
      (thread-join
        (fork-thread
          (lambda ()
            (let ([engine
                   (make-engine
                     (lambda ()
                       (set! adopt-blocked?
                         (guard (condition [else #t])
                           (#3%$native-fiber-adopt 0)
                           #f))))])
              (engine 10000
                (lambda args (set! worker-result #t))
                (lambda args (set! worker-result #f)))))))
      (and worker-result
           engine-blocked?
           adopt-blocked?
           (let ([finished (#3%$native-fiber-switch scheduler task 'start)])
             (equal? finished '(return . done)))
           task-blocked?
           (eq? (#3%$native-fiber-state task) 'finished)))))

(mat native-fiber-safe-point-preemption
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [current-scheduler scheduler]
         [task #f]
         [progress #f]
         [preempt-progress #f]
         [handler-count 0]
         [handler-returned? #f]
         [nested-deferred? #f]
         [handler-thread #f]
         [resume-thread #f]
         [saved-handler (timer-interrupt-handler)])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (set-timer 1000)
          (let loop ([i 1000000])
            (set! progress i)
            (unless (fx= i 0)
              (loop (fx- i 1))))
          (set! resume-thread (get-thread-id))
          'completed)
        (native-fiber-dynamic-return-handler (lambda () current-scheduler))
        #b100))
    (dynamic-wind
      (lambda ()
        (timer-interrupt-handler
          (lambda ()
            (set! handler-count (fx+ handler-count 1))
            (set! handler-thread (get-thread-id))
            (set! preempt-progress progress)
            (if (#3%$native-fiber-preempt scheduler 'preempted)
                (begin
                  (set! nested-deferred?
                    (#3%$call-with-native-fiber-preemption-window
                      (lambda ()
                        (and (not (#3%$native-fiber-preempt
                                    scheduler 'duplicate))
                             (not (#3%$native-fiber-service-preemption))))))
                  (set! handler-returned? #t))
                (set-timer 1)))))
      (lambda ()
        (#3%$native-fiber-try-claim! task)
        (let ([preempted (#3%$native-fiber-switch scheduler task 'start)])
          (and (eq? preempted 'preempted)
               handler-returned?
               nested-deferred?
               (fixnum? preempt-progress)
               (fx> preempt-progress 0)
               (fx< preempt-progress 1000000)
               (fx= handler-count 1)
               (fx= handler-thread (get-thread-id))
               (let ([worker-result #f])
                 (thread-join
                   (fork-thread
                     (lambda ()
                       (let ([worker (#3%$native-fiber-adopt 0)])
                         (set! current-scheduler worker)
                         (set! worker-result
                           (and (#3%$native-fiber-try-claim! task)
                                (equal?
                                  (#3%$native-fiber-switch worker task 'resume)
                                  '(return . completed))
                                (fx= resume-thread (get-thread-id))))))))
                 worker-result)
               (not (fx= resume-thread handler-thread))
               (eq? (#3%$native-fiber-state task) 'finished))))
      (lambda ()
        (set-timer 0)
        (timer-interrupt-handler saved-handler)))))

(mat native-fiber-preemption-handler-unwind
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [task #f]
         [requested? #f]
         [caught? #f]
         [request-cleared? #f]
         [saved-handler (timer-interrupt-handler)])
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (guard (condition
                   [else
                    (set! caught? #t)])
            (set-timer 1000)
            (let loop ([i 1000000])
              (unless (fx= i 0)
                (loop (fx- i 1)))))
          (set! request-cleared?
            (and (not (#3%$native-fiber-preempt-active))
                 (not (#3%$native-fiber-preempt-target))
                 (not (#3%$native-fiber-preempt-payload))))
          'completed)
        (native-fiber-return-handler scheduler)
        #b001))
    (dynamic-wind
      (lambda ()
        (timer-interrupt-handler
          (lambda ()
            (set! requested?
              (#3%$native-fiber-preempt scheduler 'must-not-be-delivered))
            (error 'native-fiber-preemption-handler-unwind
              "timer handler unwind"))))
      (lambda ()
        (#3%$native-fiber-try-claim! task)
        (and (equal? (#3%$native-fiber-switch scheduler task 'start)
                     '(return . completed))
             requested?
             caught?
             request-cleared?
             (eq? (#3%$native-fiber-state task) 'finished)))
      (lambda ()
        (set-timer 0)
        (timer-interrupt-handler saved-handler)))))

(mat native-fiber-runtime-exit-boundary
  (let* ([scheduler
          (or (#3%$current-native-fiber) (#3%$native-fiber-adopt 0))]
         [task #f]
         [handlers-ran 0])
    (define (blocked? thunk)
      (guard (condition [else #t])
        (thunk)
        #f))
    (set! task
      (#3%$native-fiber-create
        (lambda ()
          (parameterize ([exit-handler
                          (lambda args
                            (set! handlers-ran (fx+ handlers-ran 1)))]
                         [reset-handler
                          (lambda ()
                            (set! handlers-ran (fx+ handlers-ran 1)))]
                         [abort-handler
                          (lambda args
                            (set! handlers-ran (fx+ handlers-ran 1)))])
            (and (blocked? (lambda () (exit 0)))
                 (blocked? reset)
                 (blocked? abort)
                 (fx= handlers-ran 0))))
        (native-fiber-return-handler scheduler)
        #b001))
    (#3%$native-fiber-try-claim! task)
    (let ([finished (#3%$native-fiber-switch scheduler task 'start)])
      (and (equal? finished '(return . #t))
           (fx= handlers-ran 0)
           (eq? (#3%$native-fiber-state task) 'finished)))))

(mat wait-for-threads
  (begin
    ;; To avoid breaking later tests that use `(collect)`,
    ;; wait for any threads created here to exit
    (let ()
      (define $threads (foreign-procedure "(cs)threads" () scheme-object))
      (let loop ()
        (unless (= 1 (length ($threads)))
          (sleep (make-time 'time-duration 10000 0))
          (loop))))
    #t)
)

)
