;;; async.ms
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
(import (chezscheme async channels))
(import (chezscheme control))

(define async-test-burn
  (lambda (n)
    (let loop ([i 0])
      (unless (fx= i n)
        (loop (fx+ i 1))))))

(mat async

  ;; ---- basic scheduling

  (equal? (run-async (lambda () 42)) 42)
  (equal? (call-with-values (lambda () (run-async (lambda () (values 1 2 3)))) list)
    '(1 2 3))
  (equal? (call-with-values (lambda () (run-async (lambda () (values)))) list)
    '())
  (equal? (run-async (lambda () (let ([t (spawn-task (lambda () (+ 1 2)))])
                                  (task-join t))))
    3)
  (equal? (run-async (lambda ()
                       (call-with-values
                         (lambda () (task-join (spawn-task (lambda () (values 'a 'b)))))
                         list)))
    '(a b))
  (equal? (run-async (lambda ()
                       (call-with-values
                         (lambda () (task-join (spawn-task (lambda () (values)))))
                         list)))
    '())
  (equal? (run-async
            (lambda ()
              (let ([tasks
                     (let loop ([i 0] [tasks '()])
                       (if (fx= i 8)
                           tasks
                           (loop (fx+ i 1)
                             (cons
                               (spawn-task
                                 (let ([i i])
                                   (lambda () (task-yield) i)))
                               tasks))))])
                (let loop ([tasks tasks] [values '()])
                  (if (null? tasks)
                      values
                      (loop (cdr tasks)
                        (cons (task-join (car tasks)) values)))))))
    '(0 1 2 3 4 5 6 7))

  ;; user prompts cannot intercept scheduler suspension
  (equal? (reset (run-async (lambda () (task-yield) 'private-prompt)))
    'private-prompt)

  ;; ---- task accessors

  (equal? (run-async
            (lambda ()
              (let ([t (spawn-task (lambda () 'x) 'name 'worker)])
                (list (task? t)
                      (fixnum? (task-id t))
                      (eq? (task-name t) 'worker)
                      (task? 'no)
                      (begin (task-join t) (task-state t))))))
    '(#t #t #t #f completed))

  ;; ---- errors

  (error? (perform-operation (always-operation 1)))
  (error? (spawn-task (lambda () 1)))
  (error? (run-async (lambda () (task-join (current-async-task)))))
  (error? (run-async (lambda () (spawn-task 'not-a-procedure))))
  (error? (run-async (lambda () (spawn-task (lambda () 1) 'name))))
  (error? (run-async (lambda () (spawn-task (lambda () 1) 'bogus-option 1))))
  (error? (run-async (lambda () 1) 'clock 'bogus))
  (error? (run-async (lambda () 1) 'parallelism 0))
  (error? (run-async (lambda () 1) 'preemption-ticks 0))
  (error? (run-async (lambda () 1) 'preemption-ticks -1))
  (error? (run-async (lambda () 1) 'preemption-ticks 'often))
  (error? (run-async (lambda () 1) 'clock 'virtual 'parallelism 2))
  (error? (make-channel -1))
  (error? (run-async (lambda () (channel-put 'not-a-channel 1))))
  (error? (run-async (lambda () (task-join 'not-a-task))))
  (error? (run-async (lambda () (async-sleep -1))))

  ;; ---- yield and fairness

  (equal? (task-yield) #f)
  (equal? (run-async
            (lambda ()
              (let ([log '()])
                (spawn-task (lambda () (set! log (cons 'a1 log)) (task-yield) (set! log (cons 'a2 log))))
                (spawn-task (lambda () (set! log (cons 'b1 log)) (task-yield) (set! log (cons 'b2 log))))
                (task-yield) (task-yield) (task-yield)
                (reverse log))))
    '(a1 b1 a2 b2))
  (equal? (run-async
            (lambda ()
              (let ([log '()])
                (spawn-task (lambda () (set! log (cons 1 log))))
                (spawn-task (lambda () (set! log (cons 2 log))))
                (spawn-task (lambda () (set! log (cons 3 log))))
                (task-yield)
                (reverse log))))
    '(1 2 3))

  ;; a yielded task is re-queued for the next turn behind tasks already
  ;; pending there, so a second yield lets both children finish
  (equal? (run-async
            (lambda ()
              (let ([n 0])
                (spawn-task (lambda () (set! n (+ n 1)) (task-yield) (set! n (+ n 1))))
                (spawn-task (lambda () (set! n (+ n 10)) (task-yield) (set! n (+ n 10))))
                (task-yield)
                (task-yield)
                n)))
    22)

  ;; ---- timed preemption

  ;; Cooperative mode remains the default.  With a bounded engine turn, a
  ;; CPU-bound root task gives a ready child a scheduler turn.
  (equal?
    (list
      (run-async
        (lambda ()
          (let ([ran? (box #f)])
            (spawn-task (lambda () (set-box! ran? #t)))
            (async-test-burn 100000)
            (unbox ran?))))
      (run-async
        (lambda ()
          (let ([ran? (box #f)])
            (spawn-task (lambda () (set-box! ran? #t)))
            (async-test-burn 100000)
            (unbox ran?)))
        'preemption-ticks 10))
    '(#f #t))

  ;; Engine expiration preserves values and composes with an explicit async
  ;; suspension later in the same task.
  (equal?
    (call-with-values
      (lambda ()
        (run-async
          (lambda ()
            (async-test-burn 100000)
            (async-sleep 1)
            (async-test-burn 100000)
            (values 'a 'b 'c))
          'clock 'virtual
          'preemption-ticks 20))
      list)
    '(a b c))

  ;; Exception handlers and fiber-local parameters survive repeated engine
  ;; expiration and resumption.
  (equal?
    (run-async
      (lambda ()
        (let ([p (make-thread-parameter 'outer)])
          (parameterize ([p 'inner])
            (guard (c [else (list (p) (condition-message c))])
              (async-test-burn 100000)
              (error 'preemption-test "boom")))))
      'preemption-ticks 10)
    '(inner "boom"))

  ;; async-aware winders run for the user extent, not for timer-driven
  ;; continuation transfers.
  (equal?
    (run-async
      (lambda ()
        (let ([before 0] [after 0])
          (async-dynamic-wind
            (lambda () (set! before (fx+ before 1)))
            (lambda () (async-test-burn 100000))
            (lambda () (set! after (fx+ after 1))))
          (list before after)))
      'preemption-ticks 10)
    '(1 1))

  ;; A scheduler entered by preempted user code would nest engines and is
  ;; rejected before it can capture scheduler internals.
  (error?
    (run-async
      (lambda () (run-async (lambda () 'nested)))
      'preemption-ticks 10))

  ;; ---- timers (virtual clock)

  (equal? (run-async (lambda () (async-sleep 10) 'done) 'clock 'virtual) 'done)
  (equal? (run-async
            (lambda ()
              (let ([log '()])
                (spawn-task (lambda () (async-sleep 2) (set! log (cons 'late log))))
                (spawn-task (lambda () (async-sleep 1) (set! log (cons 'early log))))
                (async-sleep 3)
                (reverse log)))
            'clock 'virtual)
    '(early late))
  (equal? (run-async (lambda () (async-sleep 0) 'done)) 'done)

  ;; timers with the same deadline fire in registration order
  (equal? (run-async
            (lambda ()
              (let ([log '()])
                (spawn-task (lambda () (async-sleep 1) (set! log (cons 1 log))))
                (spawn-task (lambda () (async-sleep 1) (set! log (cons 2 log))))
                (spawn-task (lambda () (async-sleep 1) (set! log (cons 3 log))))
                (async-sleep 2)
                (reverse log)))
            'clock 'virtual)
    '(1 2 3))

  ;; ---- operations

  (equal? (run-async (lambda () (perform-operation (always-operation 'x)))) 'x)
  (equal? (run-async (lambda ()
                       (call-with-values
                         (lambda () (perform-operation (always-operation 1 2)))
                         list)))
    '(1 2))
  (equal? (run-async (lambda () (perform-operation (wrap-operation (always-operation 21) (lambda (x) (* x 2))))))
    42)
  (equal? (run-async
            (lambda ()
              (perform-operation
                (wrap-operation
                  (wrap-operation (always-operation 10) (lambda (x) (+ x 1)))
                  (lambda (x) (* x 2))))))
    22)
  (equal? (run-async (lambda ()
                       (perform-operation
                         (choice-operation (never-operation)
                                           (always-operation 'pick-me)
                                           (never-operation)))))
    'pick-me)
  (equal? (run-async (lambda ()
                       (perform-operation
                         (choice-operation (always-operation 'a) (always-operation 'b)))))
    'a)
  (equal? (run-async
            (lambda ()
              (perform-operation
                (choice-operation
                  (never-operation)
                  (wrap-operation (sleep-operation 5) (lambda () 'timed-out)))))
            'clock 'virtual)
    'timed-out)

  ;; immediate completion does not capture a continuation
  (equal? (run-async
            (lambda ()
              (let ([s (current-async-scheduler)])
                (let ([n (async-scheduler-suspension-count s)])
                  (perform-operation (always-operation 'x))
                  (= n (async-scheduler-suspension-count s))))))
    #t)

  ;; a blocked operation can claim its resumption only once
  (equal? (run-async
            (lambda ()
              (let ([deliver-box (box #f)] [runs 0])
                (let* ([op
                        (make-operation
                          (lambda (ss) #f)
                          (lambda (ss deliver)
                            (set-box! deliver-box deliver)
                            '(test-operation)))]
                       [task
                        (spawn-task
                          (lambda ()
                            (let ([v (perform-operation op)])
                              (set! runs (+ runs 1))
                              v)))])
                  (task-yield)
                  (let* ([first ((unbox deliver-box) (cons 'values '(first)))]
                         [second ((unbox deliver-box) (cons 'values '(second)))])
                    (list first second (task-join task) runs))))))
    '(#t #f first 1))

  ;; a choice nacks every registered loser, but not the winner
  (equal? (run-async
            (lambda ()
              (let ([deliver1 (box #f)] [deliver2 (box #f)]
                    [nack1 0] [nack2 0])
                (define (test-operation deliver-box nack!)
                  (make-operation
                    (lambda (ss) #f)
                    (lambda (ss deliver)
                      (set-box! deliver-box deliver)
                      '(test-choice))
                    (lambda (vals) vals)
                    (lambda (ss) (nack!))))
                (let* ([op1 (test-operation deliver1
                              (lambda () (set! nack1 (+ nack1 1))))]
                       [op2 (test-operation deliver2
                              (lambda () (set! nack2 (+ nack2 1))))]
                       [task (spawn-task
                               (lambda ()
                                 (perform-operation
                                   (choice-operation op1 op2))))])
                  (task-yield)
                  ((unbox deliver1) (cons 'values '(winner)))
                  (let ([late ((unbox deliver2) (cons 'values '(loser)))])
                    (list (task-join task) late nack1 nack2))))))
    '(winner #f 0 1))

  ;; ---- channels

  (equal? (run-async
            (lambda ()
              (let ([ch (make-channel)])
                (let ([t (spawn-task (lambda () (channel-get ch)))])
                  (channel-put ch 'hello)
                  (task-join t)))))
    'hello)
  (equal? (run-async
            (lambda ()
              (let ([ch (make-channel 2)])
                (channel-put ch 1)
                (channel-put ch 2)
                (list (channel-get ch) (channel-get ch)))))
    '(1 2))

  ;; bounded channel backpressure
  (equal? (run-async
            (lambda ()
              (let ([ch (make-channel 1)] [log '()])
                (channel-put ch 'a)
                (spawn-task (lambda ()
                              (set! log (cons (channel-get ch) log))
                              (set! log (cons (channel-get ch) log))))
                (channel-put ch 'b)
                (reverse log))))
    '(a b))

  ;; choice: exactly one winner, losing waiter withdrawn
  (equal? (run-async
            (lambda ()
              (let ([ch1 (make-channel)] [ch2 (make-channel)])
                (spawn-task (lambda () (channel-put ch1 'one)))
                (spawn-task (lambda () (channel-put ch2 'two)))
                (let* ([first (perform-operation
                                (choice-operation (channel-get-operation ch1)
                                                  (channel-get-operation ch2)))]
                       [second (perform-operation
                                 (choice-operation (channel-get-operation ch1)
                                                   (channel-get-operation ch2)))])
                  (list first second)))))
    '(one two))

  ;; channel vs timer race on the virtual clock
  (equal? (run-async
            (lambda ()
              (let ([ch (make-channel)])
                (spawn-task (lambda () (async-sleep 1) (channel-put ch 'data)))
                (perform-operation
                  (choice-operation
                    (channel-get-operation ch)
                    (wrap-operation (sleep-operation 5) (lambda () 'timed-out))))))
            'clock 'virtual)
    'data)
  (equal? (run-async
            (lambda ()
              (let ([ch (make-channel)])
                (perform-operation
                  (choice-operation
                    (channel-get-operation ch)
                    (wrap-operation (sleep-operation 5) (lambda () 'timed-out))))))
            'clock 'virtual)
    'timed-out)

  ;; ---- futures

  (equal? (run-async
            (lambda ()
              (let ([f (make-future)])
                (spawn-task (lambda () (future-fulfil! f 99)))
                (future-get f))))
    99)
  (equal? (run-async
            (lambda ()
              (let ([f (make-future)])
                (spawn-task (lambda () (future-fulfil! f 1 2)))
                (call-with-values (lambda () (future-get f)) list))))
    '(1 2))
  (equal? (run-async
            (lambda ()
              (let ([f (make-future)])
                (future-fulfil! f 'done)
                (list (future-get f) (future-get f)))))
    '(done done))
  (equal? (run-async
            (lambda ()
              (let ([f (make-future)] [c0 (make-message-condition "nope")])
                (spawn-task (lambda () (future-fail! f c0)))
                (guard (c [else (eq? c c0)]) (future-get f)))))
    #t)
  ;; An exceptional completion resumes on the wait owner before raising
  ;; through the captured fiber exception context.
  (or (not (threaded?))
      (let loop ([i 0])
        (or (fx= i 32)
            (and
              (equal?
                (run-async
                  (lambda ()
                    (let ([started (make-future)] [blocked (make-future)])
                      (let ([task
                             (spawn-task
                               (lambda ()
                                 (future-fulfil! started)
                                 (guard (c [else (condition-message c)])
                                   (future-get blocked)))
                               'migratable? #t)])
                        (future-get started)
                        (future-fail! blocked
                          (make-message-condition "remote failure"))
                        (task-join task))))
                  'parallelism 4)
                "remote failure")
              (loop (fx+ i 1))))))
  (error? (run-async (lambda () (let ([f (make-future)])
                                  (future-fulfil! f 1)
                                  (future-fulfil! f 2)))))

  ;; a blocked operation applies its wrapper in the resumed fiber, not in the
  ;; thread that completes the operation
  (equal? (let ([p (make-thread-parameter 'outside)])
            (run-async
              (lambda ()
                (p 'fiber)
                (let ([f (make-future)])
                  (fork-thread
                    (lambda ()
                      (p 'foreign-thread)
                      (sleep (make-time 'time-duration 1000000 0))
                      (future-fulfil! f 'value)))
                  (perform-operation
                    (wrap-operation (future-operation f)
                      (lambda (v) (list v (p)))))))))
    '(value fiber))

  ;; completion and cancellation race through the same one-shot claim
  (equal? (run-async
            (lambda ()
              (let ([f (make-future)])
                (let ([completed-first
                       (spawn-task (lambda () (future-get f)))])
                  (task-yield)
                  (future-fulfil! f 'completed)
                  (task-cancel! completed-first)
                  (let ([first-result
                         (guard (c
                                  [(async-cancellation-condition? c)
                                   'canceled])
                           (task-join completed-first))]
                        [f2 (make-future)])
                    (let ([canceled-first
                           (spawn-task (lambda () (future-get f2)))])
                      (task-yield)
                      (task-cancel! canceled-first)
                      (future-fulfil! f2 'too-late)
                      (list first-result
                            (guard (c
                                     [(async-cancellation-condition? c)
                                      'canceled])
                              (task-join canceled-first)))))))))
    '(canceled canceled))

  ;; ---- joining and exception propagation

  (equal? (run-async
            (lambda ()
              (let ([c0 (make-message-condition "boom")])
                (let ([t (spawn-task (lambda () (raise c0)))])
                  (guard (c [else (eq? c c0)])
                    (task-join t))))))
    #t)

  ;; multiple joiners all observe failure
  (equal? (run-async
            (lambda ()
              (let ([t (spawn-task (lambda () (raise (make-message-condition "x"))))])
                (let ([j1 (spawn-task (lambda () (guard (c [else 'j1-saw]) (task-join t))))]
                      [j2 (spawn-task (lambda () (guard (c [else 'j2-saw]) (task-join t))))])
                  (list (task-join j1) (task-join j2))))))
    '(j1-saw j2-saw))

  ;; multiple joiners receive the same successful multiple-value result
  (equal? (run-async
            (lambda ()
              (let ([t (spawn-task (lambda () (task-yield) (values 'a 'b)))])
                (let ([j1 (spawn-task
                            (lambda ()
                              (call-with-values (lambda () (task-join t)) list)))]
                      [j2 (spawn-task
                            (lambda ()
                              (call-with-values (lambda () (task-join t)) list)))])
                  (list (task-join j1) (task-join j2))))))
    '((a b) (a b)))

  ;; ---- cancellation

  (equal? (run-async
            (lambda ()
              (let ([t (spawn-task (lambda () (async-sleep 100) 'survived))])
                (task-cancel! t)
                (guard (c [(async-cancellation-condition? c) 'canceled])
                  (task-join t)
                  'joined))))
    'canceled)
  (equal? (run-async
            (lambda ()
              (let ([t (spawn-task (lambda () (async-sleep 100)))])
                (task-cancel! t 'because)
                (guard (c [(async-cancellation-condition? c) (async-cancellation-reason c)])
                  (task-join t)))))
    'because)

  ;; a ready task observes cancellation before running user code
  (equal? (run-async
            (lambda ()
              (let ([ran? #f])
                (let ([t (spawn-task (lambda () (set! ran? #t)))])
                  (task-cancel! t)
                  (guard (c [else #f]) (task-join t))
                  ran?))))
    #f)

  ;; a running task observes cancellation at its next cancellation point
  (equal? (run-async
            (lambda ()
              (let ([t (spawn-task (lambda ()
                                     (guard (c [(async-cancellation-condition? c) 'got-it])
                                       (task-yield)
                                       (task-yield)
                                       'no-cancel)))])
                (task-yield)
                (task-cancel! t)
                (task-join t))))
    'got-it)

  ;; cancellation is idempotent
  (equal? (run-async
            (lambda ()
              (let ([t (spawn-task (lambda () (async-sleep 100)))])
                (task-cancel! t 'first)
                (task-cancel! t 'second)
                (guard (c [(async-cancellation-condition? c) (async-cancellation-reason c)])
                  (task-join t)))))
    'first)

  ;; cancellation propagates to descendants
  (equal? (run-async
            (lambda ()
              (let ([t1 (spawn-task
                          (lambda ()
                            (spawn-task (lambda () (async-sleep 100)))
                            (async-sleep 100)))])
                (task-yield)
                (task-cancel! t1)
                (guard (c [(async-cancellation-condition? c) 'canceled])
                  (task-join t1)))))
    'canceled)

  ;; a parent becomes canceled only after descendants finish cancellation
  ;; cleanup
  (equal? (run-async
            (lambda ()
              (let ([child-box (box #f)] [cleaned? (box #f)])
                (let ([parent
                       (spawn-task
                         (lambda ()
                           (let ([child
                                  (spawn-task
                                    (lambda ()
                                      (guard (c
                                               [(async-cancellation-condition? c)
                                                (set-box! cleaned? #t)
                                                (raise c)])
                                        (let loop () (task-yield) (loop)))))])
                             (set-box! child-box child)
                             (let loop () (task-yield) (loop)))))])
                  (task-yield)
                  (task-yield)
                  (task-cancel! parent)
                  (guard (c [else #f]) (task-join parent))
                  (list (unbox cleaned?)
                        (task-state (unbox child-box))
                        (task-state parent))))))
    '(#t canceled canceled))

  ;; ---- task groups

  (equal? (run-async
            (lambda ()
              (let ([g (make-task-group)])
                (spawn-task (lambda () 'x) 'group g)
                (spawn-task (lambda () (task-yield) 'y) 'group g)
                (task-group-wait g)
                'done)))
    'done)

  ;; unobserved child failure is reported to its owning group
  (equal? (run-async
            (lambda ()
              (let ([g (make-task-group)])
                (spawn-task (lambda () (raise (make-message-condition "x"))) 'group g)
                (guard (c [(message-condition? c) 'reported])
                  (task-group-wait g)))))
    'reported)

  ;; Observed failures do not hide older unobserved failures in the group.
  (equal? (run-async
            (lambda ()
              (let ([g (make-task-group)])
                (let ([unobserved
                       (spawn-task
                         (lambda () (raise (make-message-condition "unobserved")))
                         'group g)]
                      [observed
                       (spawn-task
                         (lambda ()
                           (task-yield)
                           (raise (make-message-condition "observed")))
                         'group g)])
                  (guard (c [else (void)]) (task-join observed))
                  (guard (c [(and (message-condition? c)
                                  (string=? (condition-message c) "unobserved"))
                             'reported])
                    (task-group-wait g))))))
    'reported)

  ;; A foreign completion cannot be lost between the scheduler's queue check
  ;; and its condition wait.
  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let loop ([i 0])
              (if (fx= i 200)
                  'done
                  (let ([f (make-future)])
                    (fork-thread (lambda () (future-fulfil! f i)))
                    (unless (fx= (future-get f) i)
                      (errorf 'foreign-completion "wrong future value"))
                    (loop (fx+ i 1)))))))
        'done))

  ;; unobserved child failure fails the draining root task
  (equal? (guard (c [(message-condition? c) 'failed-as-expected])
            (run-async
              (lambda ()
                (spawn-task (lambda () (errorf 'child "boom")))
                (task-yield)
                'survived)))
    'failed-as-expected)

  ;; scheduler waits for children after the root body completes
  (equal? (let ([b (box #f)])
            (run-async (lambda ()
                         (spawn-task (lambda () (task-yield) (set-box! b 'child-ran)))
                         'body-done))
            (unbox b))
    'child-ran)

  ;; a failing group body cancels children and waits for their cleanup
  (equal? (let ([cleaned? (box #f)])
            (guard (c [else #f])
              (run-async
                (lambda ()
                  (spawn-task
                    (lambda ()
                      (guard (c
                               [(async-cancellation-condition? c)
                                (set-box! cleaned? #t)
                                (raise c)])
                        (let loop () (task-yield) (loop)))))
                  (task-yield)
                  (raise (make-message-condition "root failed")))))
            (unbox cleaned?))
    #t)

  ;; ---- dynamic-wind interaction

  ;; async-dynamic-wind guards are skipped during scheduling switches
  (equal? (run-async
            (lambda ()
              (let ([befores 0] [afters 0])
                (async-dynamic-wind
                  (lambda () (set! befores (+ befores 1)))
                  (lambda () (task-yield) 'x)
                  (lambda () (set! afters (+ afters 1))))
                (list befores afters))))
    '(1 1))

  ;; async-dynamic-wind cleanup runs on cancellation
  (equal? (run-async
            (lambda ()
              (let ([afters 0])
                (let ([t (spawn-task
                           (lambda ()
                             (async-dynamic-wind
                               (lambda () #f)
                               (lambda () (async-sleep 100))
                               (lambda () (set! afters (+ afters 1))))))])
                  (task-yield)
                  (task-cancel! t)
                  (guard (c [else #f]) (task-join t))
                  afters))))
    1)

  ;; ordinary dynamic-wind retains its semantics across suspension
  (equal? (run-async
            (lambda ()
              (let ([befores 0] [afters 0])
                (dynamic-wind
                  (lambda () (set! befores (+ befores 1)))
                  (lambda () (task-yield))
                  (lambda () (set! afters (+ afters 1))))
                (list befores afters))))
    '(2 2))

  ;; ---- fiber-local dynamic state

  (equal? (let ([p (make-thread-parameter 'parent)])
            (run-async
              (lambda ()
                (p 'in-root)
                (let ([t (spawn-task (lambda ()
                                       (let ([inherited (p)])
                                         (p 'child-mutated)
                                         inherited)))])
                  (let ([inherited (task-join t)])
                    (list inherited (p)))))))
    '(in-root in-root))

  ;; a thread parameter allocated by one task starts at its initial value in
  ;; siblings whose snapshots predate the allocation
  (equal? (run-async
            (lambda ()
              (let ([pbox (box #f)])
                (let ([a (spawn-task
                           (lambda ()
                             (let ([p (make-thread-parameter 'initial)])
                               (set-box! pbox p)
                               (p 'task-a)
                               (task-yield)
                               (p))))]
                      [b (spawn-task
                           (lambda ()
                             (task-yield)
                             ((unbox pbox))))])
                  (list (task-join a) (task-join b))))))
    '(task-a initial))

  ;; scheduler exit restores the caller's thread-parameter state
  (equal? (let ([p (make-thread-parameter 'outside)])
            (run-async (lambda () (p 'inside) (task-yield) (p)))
            (p))
    'outside)

  ;; ordinary parameterize state rides each task's continuation
  (equal? (let ([p (make-parameter 'default)])
            (run-async
              (lambda ()
                (parameterize ([p 'root])
                  (let ([t (spawn-task (lambda ()
                                         (parameterize ([p 'child])
                                           (task-yield)
                                           (p))))])
                    (task-yield)
                    (list (p) (task-join t)))))))
    '(root child))

  ;; exception handler stacks are fiber-local across scheduler switches
  (equal? (run-async
            (lambda ()
              (let ([root-state
                     (create-exception-state (lambda (c) 'root-handler))]
                    [child-state
                     (create-exception-state (lambda (c) 'child-handler))])
                (current-exception-state root-state)
                (let ([child
                       (spawn-task
                         (lambda ()
                           (current-exception-state child-state)
                           (task-yield)
                           (raise-continuable 'child-probe)))])
                  (task-yield)
                  (list (raise-continuable 'root-probe)
                        (task-join child))))))
    '(root-handler child-handler))

  ;; ---- remote wakeup from a plain thread

  (equal? (run-async
            (lambda ()
              (let ([f (make-future)])
                (fork-thread
                  (lambda ()
                    (sleep (make-time 'time-duration 1000000 0))
                    (future-fulfil! f 'from-thread)))
                (future-get f))))
    'from-thread)

  ;; ---- scheduler groups

  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let* ([n 6]
                   [tasks (make-vector n)]
                   [results (make-vector n #f)]
                   [gate (make-future)]
                   [mutex (make-mutex)]
                   [condition (make-condition)]
                   [parameter (make-thread-parameter 'unset)]
                   [started 0])
              (do ([i 0 (fx+ i 1)]) ((fx= i n))
                (vector-set! tasks i
                  (spawn-task
                    (let ([i i])
                      (lambda ()
                        (let ([scheduler (current-async-scheduler)]
                              [thread-id (get-thread-id)]
                              [migrated? #f])
                          (parameter i)
                          ;; Hold one task on each scheduler until all three
                          ;; scheduler threads have entered user code.
                          (with-mutex mutex
                            (set! started (fx+ started 1))
                            (when (fx= started 3)
                              (condition-broadcast condition))
                            (let loop ()
                              (unless (fx>= started 3)
                                (condition-wait condition mutex)
                                (loop))))
                          (future-get gate)
                          (do ([j 0 (fx+ j 1)]) ((fx= j 16))
                            (task-yield)
                            (when (or (not (eq? scheduler
                                                 (current-async-scheduler)))
                                      (not (fx= thread-id (get-thread-id))))
                              (set! migrated? #t)))
                          (vector-set! results i
                            (list (async-scheduler? scheduler)
                                  (eq? (task-scheduler (current-async-task))
                                       (current-async-scheduler))
                                  (fx= (parameter) i)
                                  migrated?)))))
                    'migratable? #t)))
              (let loop ()
                (unless (with-mutex mutex (fx= started n))
                  (task-yield)
                  (loop)))
              ;; Completing a future publishes parked tasks to the group.
              (future-fulfil! gate 'go)
              (do ([i 0 (fx+ i 1)]) ((fx= i n))
                (task-join (vector-ref tasks i)))
              (let ([ids (make-eqv-hashtable)]
                    [schedulers (vector-map task-scheduler tasks)])
                (vector-for-each
                  (lambda (task) (hashtable-set! ids (task-id task) #t))
                  tasks)
                (list
                  (for-all (lambda (r) (equal? (list-head r 3) '(#t #t #t)))
                    (vector->list results))
                  (exists cadddr (vector->list results))
                  (fx= (hashtable-size ids) n)
                  (exists
                    (lambda (scheduler)
                      (not (eq? scheduler (vector-ref schedulers 0))))
                    (cdr (vector->list schedulers)))))))
          'parallelism 3)
        '(#t #t #t #t)))

  ;; Channel delivery can cross scheduler threads and resume a migratable
  ;; continuation on any scheduler in the group.
  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let ([ch (make-channel)]
                  [mutex (make-mutex)]
                  [condition (make-condition)]
                  [started 0])
              (define rendezvous
                (lambda ()
                  (with-mutex mutex
                    (set! started (fx+ started 1))
                    (when (fx= started 2)
                      (condition-broadcast condition))
                    (let loop ()
                      (unless (fx= started 2)
                        (condition-wait condition mutex)
                        (loop))))))
              (let ([consumer
                     (spawn-task
                       (lambda () (rendezvous) (channel-get ch))
                       'migratable? #t)]
                    [producer
                     (spawn-task
                       (lambda () (rendezvous) (channel-put ch 'cross-thread))
                       'migratable? #t)])
                (let ([value (task-join consumer)])
                  (task-join producer)
                  value))))
          'parallelism 2)
        'cross-thread))

  ;; Cancellation published from another scheduler wakes a parked waiter and
  ;; preserves the cancellation reason across resumption.
  (or (not (threaded?))
      (let loop ([i 0])
        (or (fx= i 32)
            (and
              (equal?
                (run-async
                  (lambda ()
                    (let ([started (make-future)] [blocked (make-future)])
                      (let ([task
                             (spawn-task
                               (lambda ()
                                 (future-fulfil! started)
                                 (future-get blocked))
                               'migratable? #t)])
                        (future-get started)
                        (task-cancel! task 'remote-cancel)
                        (guard (c
                                 [(async-cancellation-condition? c)
                                  (async-cancellation-reason c)])
                          (task-join task)))))
                  'parallelism 4)
                'remote-cancel)
              (loop (fx+ i 1))))))

  ;; A cancellation can overtake a normal completion after the waiter has
  ;; entered the shared ready queue.  Either result may win, and both paths
  ;; preserve a valid fiber exception context.
  (or (not (threaded?))
      (let loop ([i 0])
        (or (fx= i 64)
            (and
              (memq
                (run-async
                  (lambda ()
                    (let ([started (make-future)] [blocked (make-future)])
                      (let ([task
                             (spawn-task
                               (lambda ()
                                 (future-fulfil! started)
                                 (future-get blocked)
                                 'completed)
                               'migratable? #t)])
                        (future-get started)
                        (future-fulfil! blocked 'ready)
                        (task-cancel! task 'racing-cancel)
                        (guard (c
                                 [(async-cancellation-condition? c)
                                  'canceled])
                          (task-join task)))))
                  'parallelism 4)
                '(completed canceled))
              (loop (fx+ i 1))))))

  ;; ---- observability

  (equal? (run-async
            (lambda ()
              (let ([sched (current-async-scheduler)]
                    [me (current-async-task)])
                (spawn-task (lambda () (task-yield)))
                (task-yield)
                (list (async-scheduler? sched)
                      (task? me)
                      (eq? (task-scheduler me) sched)
                      (> (async-scheduler-turn-count sched) 0)
                      (> (async-scheduler-suspension-count sched) 0)
                      (>= (async-scheduler-task-count sched) 1)
                      (> (async-scheduler-wakeup-count sched) 0)))))
    '(#t #t #t #t #t #t #t))
  (error? (task-scheduler 'not-a-task))
  (equal? (current-async-task) #f)
  (equal? (current-async-scheduler) #f)
  (equal? (run-async
            (lambda ()
              (let ([t (spawn-task (lambda () (async-sleep 10)))])
                (task-yield)
                (let ([w (task-current-wait t)])
                  (task-cancel! t)
                  (guard (c [else #f]) (task-join t))
                  (and (pair? w) (eq? (car w) 'sleep))))))
    #t)
)
