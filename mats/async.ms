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

  ;; ---- operations

  (equal? (run-async (lambda () (perform-operation (always-operation 'x)))) 'x)
  (equal? (run-async (lambda ()
                       (call-with-values
                         (lambda () (perform-operation (always-operation 1 2)))
                         list)))
    '(1 2))
  (equal? (run-async (lambda () (perform-operation (wrap-operation (always-operation 21) (lambda (x) (* x 2))))))
    42)
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
                (let ([first (perform-operation
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
  (error? (run-async (lambda () (let ([f (make-future)])
                                  (future-fulfil! f 1)
                                  (future-fulfil! f 2)))))

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

  ;; ---- observability

  (equal? (run-async
            (lambda ()
              (let ([sched (current-async-scheduler)]
                    [me (current-async-task)])
                (spawn-task (lambda () (task-yield)))
                (task-yield)
                (list (async-scheduler? sched)
                      (task? me)
                      (> (async-scheduler-turn-count sched) 0)
                      (> (async-scheduler-suspension-count sched) 0)
                      (>= (async-scheduler-task-count sched) 1)
                      (> (async-scheduler-wakeup-count sched) 0)))))
    '(#t #t #t #t #t #t))
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
