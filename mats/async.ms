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
(import (chezscheme async context))
(import (chezscheme async sync))
(import (chezscheme async syntax))

(define async-test-burn
  (lambda (n)
    (let loop ([i 0])
      (unless (fx= i n)
        (loop (fx+ i 1))))))

(define async-test-deep-yield
  (lambda (depth)
    (if (fx= depth 0)
        (begin (task-yield) 0)
        (fx+ 1 (async-test-deep-yield (fx- depth 1))))))

(define async-test-call-at-depth
  (lambda (depth proc)
    (if (fx= depth 0)
        (proc)
        (fx+ 1 (async-test-call-at-depth (fx- depth 1) proc)))))

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
  (error? (run-async (lambda () 1) 'preemption-ticks 999))
  (error? (run-async (lambda () 1) 'preemption-ticks 'often))
  (error? (run-async (lambda () 1) 'clock 'virtual 'parallelism 2))
  (error? (make-channel -1))
  (error? (run-async (lambda () (channel-put 'not-a-channel 1))))
  (error? (run-async (lambda () (task-join 'not-a-task))))
  (error? (run-async (lambda () (async-sleep -1))))
  (error? (make-async-context 'not-a-context))
  (error? (async-context-cancel! 'not-a-context))
  (error? (async-context-with-timeout (make-async-context #f) -1))
  (error? (async-context-with-timeout (make-async-context #f) 1))
  (error? (call-with-async-timeout 1 (lambda () 'outside)))

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

  ;; Cooperative mode remains the default.  With a bounded native fiber turn,
  ;; a CPU-bound root task gives a ready child a scheduler turn.
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
        'preemption-ticks 1000))
    '(#f #t))

  ;; Native preemption preserves values and composes with an explicit async
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
          'preemption-ticks 1000))
      list)
    '(a b c))

  ;; Exception handlers and dynamically scoped parameters survive repeated
  ;; native preemption and resumption.
  (equal?
    (run-async
      (lambda ()
        (let ([p (make-thread-parameter 'outer)])
          (parameterize ([p 'inner])
            (guard (c [else (list (p) (condition-message c))])
              (async-test-burn 100000)
              (error 'preemption-test "boom")))))
      'preemption-ticks 1000)
    '(inner "boom"))

  ;; Ordinary winders retain their language semantics across native transfers:
  ;; every unwind has a matching rewind before task code continues.
  (let ([counts
         (run-async
           (lambda ()
             (let ([before 0] [after 0])
               (dynamic-wind
                 (lambda () (set! before (fx+ before 1)))
                 (lambda () (async-test-burn 1000000))
                 (lambda () (set! after (fx+ after 1))))
               (cons before after)))
           'preemption-ticks 1000)])
    (and (fx= (car counts) 1)
         (fx= (car counts) (cdr counts))))

  ;; Async-aware winders run once for the user extent; native task switches
  ;; preserve the task's dynamic state without unwinding it.
  (equal?
    (run-async
      (lambda ()
        (let ([before 0] [after 0])
          (async-dynamic-wind
            (lambda () (set! before (fx+ before 1)))
            (lambda () (async-test-burn 100000))
            (lambda () (set! after (fx+ after 1))))
          (list before after)))
      'preemption-ticks 1000)
    '(1 1))

  ;; A dynamically installed context survives native preemption and is
  ;; restored when its async-aware dynamic extent exits.
  (equal?
    (run-async
      (lambda ()
        (let ([root (current-async-context)]
              [context (make-async-context)])
          (let ([inside
                 (call-with-async-context context
                   (lambda ()
                     (async-test-burn 100000)
                     (eq? (current-async-context) context)))])
            (list inside (eq? (current-async-context) root)))))
      'preemption-ticks 1000)
    '(#t #t))

  ;; A scheduler entered by preempted user code would compete for the same
  ;; thread timer and is rejected before it can capture scheduler internals.
  (error?
    (run-async
      (lambda () (run-async (lambda () 'nested)))
      'preemption-ticks 1000))

  ;; Engine control state is runtime-thread-local and is not fiber state.
  ;; Async schedulers reject both cooperative and preemptive engine nesting.
  (error?
    ((make-engine (lambda () (run-async (lambda () 'cooperative))))
     100000
     (lambda (ticks value) value)
     (lambda (next) 'expired)))
  (error?
    ((make-engine
       (lambda ()
         (run-async (lambda () 'nested) 'preemption-ticks 1000)))
     100000
     (lambda (ticks value) value)
     (lambda (next) 'expired)))

  ;; Engine timer and define-threaded control state are worker-local.  Invoking
  ;; an engine from either kind of async task is rejected before it can replace
  ;; the task scheduler's timer handler or suspend engine state in a fiber.
  (error?
    (run-async
      (lambda ()
        ((make-engine (lambda () (task-yield)))
         100000
         (lambda args args)
         (lambda (next) 'expired)))))
  (error?
    (run-async
      (lambda ()
        ((make-engine (lambda () (task-yield)))
         100000
         (lambda args args)
         (lambda (next) 'expired)))
      'parallelism 4
      'preemption-ticks 1000))

  ;; A timer trap observes cancellation of CPU-bound code without requiring an
  ;; explicit async operation or cooperative yield.
  (equal?
    (run-async
      (lambda ()
        (let ([started (make-future)])
          (let ([task
                 (spawn-task
                   (lambda ()
                     (future-fulfil! started #t)
                     (let loop ()
                       (async-test-burn 10000)
                       (loop))))])
            (future-get started)
            (task-cancel! task 'cpu-cancel)
            (guard (c [(async-cancellation-condition? c)
                       (async-cancellation-reason c)])
              (task-join task)))))
      'preemption-ticks 1000)
    'cpu-cancel)

  ;; One-shot timer captures preserve a deep live stack and can later compose
  ;; with ordinary task completion.
  (equal?
    (run-async
      (lambda ()
        (async-test-call-at-depth 128
          (lambda ()
            (async-test-burn 100000)
            0)))
      'preemption-ticks 1000)
    128)

  ;; Each preempted task retains its explicitly scoped ambient context while
  ;; it remains eligible for work stealing by different scheduler threads.
  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let ([tasks
                   (let loop ([i 0] [tasks '()])
                     (if (fx= i 16)
                         tasks
                         (loop (fx+ i 1)
                           (cons
                             (spawn-task
                               (lambda ()
                                 (let ([context (make-async-context)])
                                   (call-with-async-context context
                                     (lambda ()
                                       (do ([round 0 (fx+ round 1)])
                                           ((fx= round 4))
                                         (async-test-burn 100000)
                                         (unless
                                           (eq? (current-async-context) context)
                                           (error 'preemption-context
                                             "ambient context leaked")))
                                       i))))
                               'migratable? #t)
                             tasks))))])
              (map task-join (reverse tasks))))
          'parallelism 4
          'preemption-ticks 1000)
        '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))

  ;; Scheduler turns restore the ambient timer handler they temporarily own.
  (let ([saved-handler (timer-interrupt-handler)]
        [saved-ticks (set-timer 0)]
        [handler (lambda () (void))])
    (dynamic-wind
      (lambda () (timer-interrupt-handler handler))
      (lambda ()
        (and (equal?
               (run-async
                 (lambda () (async-test-burn 100000) 'done)
                 'preemption-ticks 1000)
               'done)
             (eq? (timer-interrupt-handler) handler)
             (fx= (set-timer 0) 0)))
      (lambda ()
        (timer-interrupt-handler saved-handler)
        (set-timer saved-ticks))))

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
  (run-async
    (lambda ()
      (let* ([sched (current-async-scheduler)]
             [before (async-scheduler-suspension-count sched)])
        (async-sleep 0)
        (= before (async-scheduler-suspension-count sched)))))

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

  ;; Heap growth preserves stable ordering for a large equal-deadline batch.
  (equal? (run-async
            (lambda ()
              (let* ([log (box '())]
                    [tasks
                     (let loop ([i 0] [tasks '()])
                       (if (fx= i 128)
                           (reverse tasks)
                           (loop (fx+ i 1)
                             (cons
                               (spawn-task
                                 (lambda ()
                                   (async-sleep 1)
                                   (set-box! log (cons i (unbox log)))))
                               tasks))))])
                (for-each task-join tasks)
                (reverse (unbox log))))
            'clock 'virtual)
    (let loop ([i 0])
      (if (fx= i 128) '() (cons i (loop (fx+ i 1))))))

  ;; Canceling many sleeping tasks removes their exact heap entries.
  (equal? (run-async
            (lambda ()
              (let ([tasks
                     (let loop ([i 0] [tasks '()])
                       (if (fx= i 256)
                           tasks
                           (loop (fx+ i 1)
                             (cons (spawn-task (lambda () (async-sleep 1000)))
                               tasks))))])
                (task-yield)
                (for-each (lambda (task) (task-cancel! task 'stop)) tasks)
                (for-each
                  (lambda (task) (guard (c [else (void)]) (task-join task)))
                  tasks)
                'canceled))
            'clock 'virtual)
    'canceled)

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

  ;; immediate completion does not park the task
  (equal? (run-async
            (lambda ()
              (let ([s (current-async-scheduler)])
                (let ([n (async-scheduler-suspension-count s)])
                  (perform-operation (always-operation 'x))
                  (= n (async-scheduler-suspension-count s))))))
    #t)

  ;; a blocked operation can claim its delivery only once
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

  ;; one operation can be performed concurrently without sharing its
  ;; cancellation registration between attempts
  (equal?
    (run-async
      (lambda ()
        (let* ([op (sleep-operation 1)]
               [t1 (spawn-task (lambda () (perform-operation op) 'first))]
               [t2 (spawn-task (lambda () (perform-operation op) 'second))])
          (task-yield)
          (task-cancel! t1)
          (guard (c [(async-cancellation-condition? c) (void)])
            (task-join t1))
          (task-join t2)))
      'clock 'virtual)
    'second)

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

  ;; Closing a buffered channel preserves queued values and then reports EOF.
  (equal? (run-async
            (lambda ()
              (let ([ch (make-channel 2)])
                (channel-put ch 'a)
                (channel-put ch 'b)
                (channel-close! ch 'finished)
                (list
                  (call-with-values (lambda () (channel-receive ch)) list)
                  (call-with-values (lambda () (channel-receive ch)) list)
                  (call-with-values (lambda () (channel-receive ch)) list)
                  (channel-closed? ch)))))
    '((a #t) (b #t) (#f #f) #t))

  ;; Close is idempotent, and the first reason is retained for failed access.
  (equal? (run-async
            (lambda ()
              (let ([ch (make-channel)])
                (channel-close! ch 'first)
                (channel-close! ch 'second)
                (list
                  (guard (c [(channel-closed-condition? c)
                             (channel-closed-reason c)])
                    (channel-put ch 'late))
                  (guard (c [(channel-closed-condition? c)
                             (channel-closed-reason c)])
                    (channel-get ch))))))
    '(first first))

  ;; Closing wakes every blocked receiver and sender.
  (equal? (run-async
            (lambda ()
              (let ([receivers (make-channel)] [senders (make-channel)])
                (let ([r1 (spawn-task
                            (lambda ()
                              (call-with-values
                                (lambda () (channel-receive receivers)) list)))]
                      [r2 (spawn-task
                            (lambda ()
                              (call-with-values
                                (lambda () (channel-receive receivers)) list)))]
                      [sender (spawn-task
                                (lambda ()
                                  (guard (c [(channel-closed-condition? c)
                                             (channel-closed-reason c)])
                                    (channel-put senders 'value))))])
                  (task-yield)
                  (channel-close! receivers)
                  (channel-close! senders 'stopped)
                  (list (task-join r1) (task-join r2) (task-join sender))))))
    '((#f #f) (#f #f) stopped))

  ;; A closed receive can win a choice without leaving the other waiter live.
  (equal? (run-async
            (lambda ()
              (let ([closed (make-channel)] [open (make-channel)])
                (channel-close! closed)
                (let ([result
                       (call-with-values
                         (lambda ()
                           (perform-operation
                             (choice-operation
                               (channel-receive-operation open)
                               (channel-receive-operation closed))))
                         list)])
                  (spawn-task (lambda () (channel-put open 'still-usable)))
                  (list result (channel-get open))))))
    '((#f #f) still-usable))

  (and (channel-closed-condition? (make-channel-closed-condition 'reason))
       (eq? (channel-closed-reason (make-channel-closed-condition 'reason))
            'reason))

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

  ;; Growing the Chase--Lev rings while other schedulers steal preserves
  ;; exactly-once execution across wraparound and growth boundaries.
  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let* ([n 512]
                   [seen (make-bytevector n 0)]
                   [mutex (make-mutex)]
                   [tasks (make-vector n)])
              (do ([i 0 (fx+ i 1)]) ((fx= i n))
                (vector-set! tasks i
                  (spawn-task
                    (let ([i i])
                      (lambda ()
                        (with-mutex mutex
                          (bytevector-u8-set! seen i
                            (fx+ 1 (bytevector-u8-ref seen i))))))
                    'migratable? #t)))
              (vector-for-each task-join tasks)
              (let loop ([i 0])
                (or (fx= i n)
                    (and (fx= (bytevector-u8-ref seen i) 1)
                         (loop (fx+ i 1)))))))
          'parallelism 4)
        #t))

  ;; Compare owner-pop and thief-steal operations against a simple FIFO model
  ;; across repeated ring growth and mixed operation sequences.
  (#3%$async-debug-work-deque-model-test 4000)

  ;; Multiple native thieves race with owner push/pop while the deque grows.
  ;; Every published integer must be claimed exactly once.
  (or (not (threaded?))
      (#3%$async-debug-work-deque-contention-test 10000 6))

  ;; The lock hierarchy checker is enabled by the invariant stress runner.
  ;; In ordinary builds the hook reports that the debug-only check is dormant.
  (and (memq (#3%$async-debug-lock-rank-test) '(#t disabled)) #t)

  ;; A blocked operation applies its wrapper with the resumed task's ambient
  ;; context, not in the thread that completes the operation.
  (equal? (run-async
            (lambda ()
              (let ([context (make-async-context)]
                    [f (make-future)])
                (call-with-async-context context
                  (lambda ()
                    (fork-thread
                      (lambda ()
                        (sleep (make-time 'time-duration 1000000 0))
                        (future-fulfil! f 'value)))
                    (perform-operation
                      (wrap-operation (future-operation f)
                        (lambda (v)
                          (list v (eq? (current-async-context) context))))))))))
    '(value #t))

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

  ;; Every task owns a context, and spawned tasks receive a distinct child.
  (equal? (run-async
            (lambda ()
              (let* ([root (current-async-task)]
                     [root-context (current-async-context)]
                     [child (spawn-task
                              (lambda ()
                                (list
                                  (async-context? (current-async-context))
                                  (not (eq? root-context
                                           (current-async-context))))))])
                (list (eq? root-context (task-context root))
                      (task-join child)))))
    '(#t (#t #t)))

  ;; Cancellation propagates down the context tree, but never upward or
  ;; sideways.  The first cancellation reason wins.
  (equal? (let* ([parent (make-async-context #f)]
                 [child (make-async-context parent)]
                 [sibling (make-async-context parent)])
            (async-context-cancel! child 'child-only)
            (async-context-cancel! child 'ignored)
            (let ([before
                   (list (async-context-canceled? parent)
                         (async-context-reason child)
                         (async-context-canceled? sibling))])
              (async-context-cancel! parent 'parent-stop)
              (list before
                    (async-context-reason parent)
                    (async-context-reason child)
                    (async-context-reason sibling))))
    '((#f child-only #f) parent-stop child-only parent-stop))

  ;; A done operation is reusable and completes without values after cancel.
  (equal? (run-async
            (lambda ()
              (let* ([context (make-async-context)]
                     [done (async-context-done-operation context)]
                     [waiter (spawn-task
                               (lambda ()
                                 (call-with-values
                                   (lambda () (perform-operation done)) list)))])
                (task-yield)
                (async-context-cancel! context 'done)
                (list (task-join waiter)
                      (call-with-values
                        (lambda () (perform-operation done)) list)))))
    '(() ()))

  ;; Cancellation detaches and wakes a large context waiter set exactly once.
  (equal? (run-async
            (lambda ()
              (let* ([context (make-async-context)]
                     [started (make-async-wait-group 128)]
                     [tasks
                      (let loop ([i 0] [tasks '()])
                        (if (fx= i 128)
                            tasks
                            (loop (fx+ i 1)
                              (cons
                                (spawn-task
                                  (lambda ()
                                    (async-wait-group-done! started)
                                    (perform-operation
                                      (async-context-done-operation context))
                                    i))
                                tasks))))])
                (async-wait-group-wait started)
                (async-context-cancel! context 'fanout)
                (let ([values (map task-join tasks)])
                  (and (fx= (length values) 128)
                       (fx= (apply + values) 8128))))))
    #t)

  ;; An explicit context cancels an operation and runs the operation's nack.
  (equal? (run-async
            (lambda ()
              (let ([context (make-async-context)] [nacks 0])
                (let ([task
                       (spawn-task
                         (lambda ()
                           (guard (c [(async-cancellation-condition? c)
                                      (async-cancellation-reason c)])
                             (perform-operation/context context
                               (make-operation
                                 (lambda (ss) #f)
                                 (lambda (ss deliver) '(blocked))
                                 (lambda (values) values)
                                 (lambda (ss) (set! nacks (fx+ nacks 1))))))))])
                  (task-yield)
                  (async-context-cancel! context 'explicit-stop)
                  (list (task-join task) nacks)))))
    '(explicit-stop 1))

  ;; A dynamically installed context is inherited by ordinary child tasks.
  (equal? (run-async
            (lambda ()
              (let ([context (make-async-context)])
                (let* ([started (make-future)]
                       [task
                        (call-with-async-context context
                          (lambda ()
                            (let ([task
                                   (spawn-task
                                     (lambda ()
                                       (guard (c
                                                [(async-cancellation-condition? c)
                                                 (async-cancellation-reason c)])
                                         (future-fulfil! started)
                                         (async-sleep 100))))])
                              (future-get started)
                              (async-context-cancel! context 'inherited-stop)
                              task)))])
                  (task-join task))))
            'clock 'virtual)
    'inherited-stop)

  ;; Timeout scopes preserve successful values and cancel blocked work at the
  ;; scheduler deadline.
  (equal? (list
            (call-with-values
              (lambda ()
                (run-async
                  (lambda ()
                    (call-with-async-timeout 10
                      (lambda () (values 'a 'b))))
                  'clock 'virtual))
              list)
            (run-async
              (lambda ()
                (guard (c [(async-cancellation-condition? c)
                           (async-cancellation-reason c)])
                  (call-with-async-timeout 1
                    (lambda () (async-sleep 10) 'late))))
              'clock 'virtual))
    '((a b) deadline-exceeded))

  ;; An absolute monotonic deadline uses the real scheduler clock.
  (equal? (run-async
            (lambda ()
              (let ([context
                     (async-context-with-deadline
                       (current-async-context)
                       (current-time 'time-monotonic))])
                (guard (c [(async-cancellation-condition? c)
                           (async-cancellation-reason c)])
                  (perform-operation/context context (sleep-operation 10))))))
    'deadline-exceeded)

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

  ;; An empty explicit group detaches from its parent and can be reused in the
  ;; same structured lifetime without retaining a stale active-subgroup link.
  (equal?
    (run-async
      (lambda ()
        (let ([g (make-task-group)])
          (spawn-task (lambda () 'first) 'group g)
          (task-group-wait g)
          (let ([second (spawn-task (lambda () 'second) 'group g)])
            (task-group-wait g)
            (task-join second)))))
    'second)

  ;; Explicit groups remain part of the spawning task's lifetime even when a
  ;; child uses a cancellation context that is independent of the parent.
  (let ([finished? (box #f)])
    (and (eq?
           (run-async
             (lambda ()
               (let ([g (make-task-group)])
                 (spawn-task
                   (lambda ()
                     (async-sleep 10)
                     (set-box! finished? #t))
                   'group g
                   'context (make-async-context #f))
                 'root-returned))
             'clock 'virtual)
           'root-returned)
         (unbox finished?)))

  ;; A failure observed through an explicit group is not reported again by
  ;; the enclosing structured-concurrency boundary.
  (equal?
    (run-async
      (lambda ()
        (let ([g (make-task-group)])
          (spawn-task
            (lambda () (raise (make-message-condition "group failure")))
            'group g)
          (guard (c [(message-condition? c) 'observed])
            (task-group-wait g)))))
    'observed)

  ;; An unobserved explicit-group failure participates in the parent task's
  ;; failure boundary.
  (equal?
    (guard (c [(and (message-condition? c)
                    (string=? (condition-message c) "subgroup failure"))
               'reported])
      (run-async
        (lambda ()
          (let ([g (make-task-group)])
            (spawn-task
              (lambda () (raise (make-message-condition "subgroup failure")))
              'group g)
            'root-returned))))
    'reported)

  ;; A GC rendezvous while migratable tasks are parked validates native-fiber
  ;; roots as well as zero-frame continuation nodes in their stack links.
  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let ([ready (make-future)])
              (let ([task
                     (spawn-task
                       (lambda ()
                         (future-fulfil! ready #t)
                         (task-yield)
                         'checked)
                       'migratable? #t)])
                (future-get ready)
                (parameterize ([#%$enable-check-heap #t])
                  (collect-rendezvous))
                (task-join task))))
          'parallelism 2)
        'checked))

  ;; Explicit task groups retain the context active at group creation.
  (equal? (run-async
            (lambda ()
              (let* ([context (make-async-context)]
                     [group
                      (call-with-async-context context make-task-group)]
                     [started (make-future)]
                     [task
                      (spawn-task
                        (lambda ()
                          (guard (c [(async-cancellation-condition? c)
                                     (async-cancellation-reason c)])
                            (future-fulfil! started)
                            (async-sleep 100)))
                        'group group)])
                (future-get started)
                (async-context-cancel! context 'group-stop)
                (let ([result (task-join task)])
                  (task-group-wait group)
                  result)))
            'clock 'virtual)
    'group-stop)

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

  ;; async-dynamic-wind guards are not entered by native scheduling switches
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

  ;; ordinary dynamic-wind remains active across native-fiber suspension
  (equal? (run-async
            (lambda ()
              (let ([befores 0] [afters 0])
                (dynamic-wind
                  (lambda () (set! befores (+ befores 1)))
                  (lambda () (task-yield))
                  (lambda () (set! afters (+ afters 1))))
                (list befores afters))))
    '(1 1))

  ;; ---- task-owned dynamic state

  ;; Ambient context overrides nest dynamically and restore the task-owned
  ;; context after normal exit.
  (equal? (run-async
            (lambda ()
              (let ([root (current-async-context)]
                    [outer (make-async-context)]
                    [inner (make-async-context)])
                (list
                  (call-with-async-context outer
                    (lambda ()
                      (list
                        (eq? (current-async-context) outer)
                        (call-with-async-context inner
                          (lambda ()
                            (eq? (current-async-context) inner)))
                        (eq? (current-async-context) outer))))
                  (eq? (current-async-context) root)))))
    '((#t #t #t) #t))

  ;; Non-local exit restores the task-owned context.
  (equal? (run-async
            (lambda ()
              (let ([root (current-async-context)]
                    [override (make-async-context)])
                (guard (c [else (void)])
                  (call-with-async-context override
                    (lambda () (error 'context-test "leave scope"))))
                (eq? (current-async-context) root))))
    #t)

  ;; A child spawned under an override derives its cancellation context from
  ;; that override and remains linked after the parent leaves the scope.
  (eq? (run-async
         (lambda ()
           (let ([context (make-async-context)]
                 [started (make-future)]
                 [child #f])
             (call-with-async-context context
               (lambda ()
                 (set! child
                   (spawn-task
                     (lambda ()
                       (future-fulfil! started)
                       (guard (c
                                [(async-cancellation-condition? c)
                                 (async-cancellation-reason c)])
                         (async-sleep 60)
                         'unexpected))))
                 (future-get started)
                 (async-context-cancel! context 'inherited)))
             (task-join child))))
    'inherited)

  ;; Cooperative yield observes cancellation of the ambient context.
  (eq? (run-async
         (lambda ()
           (let ([context (make-async-context)])
             (call-with-async-context context
               (lambda ()
                 (async-context-cancel! context 'yield-canceled)
                 (guard (c
                          [(async-cancellation-condition? c)
                           (async-cancellation-reason c)])
                   (task-yield)
                   'unexpected))))))
    'yield-canceled)

  ;; Direct thread-parameter assignment has native worker semantics.  The
  ;; scheduler does not snapshot or restore the parameter vector.
  (equal? (let ([p (make-thread-parameter 'outside)])
            (let ([inside
                   (run-async
                     (lambda ()
                       (p 'inside)
                       (task-yield)
                       (p)))])
              (list inside (p))))
    '(inside inside))

  ;; Exception handler stacks remain owned by their tasks across switches.
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

  ;; A migrated, preemptible task retains both its ambient context override
  ;; and exception handler state.  Neither may come from a task that
  ;; previously occupied the receiving worker.
  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let ([tasks
                   (let loop ([i 0] [tasks '()])
                     (if (fx= i 12)
                         (reverse tasks)
                         (loop (fx+ i 1)
                           (cons
                             (spawn-task
                               (lambda ()
                                 (let ([context (make-async-context)])
                                   (call-with-async-context context
                                     (lambda ()
                                       (current-exception-state
                                         (create-exception-state
                                           (lambda (condition) i)))
                                       (do ([round 0 (fx+ round 1)])
                                           ((fx= round 8))
                                         (async-test-burn 50000)
                                         (task-yield))
                                       (list
                                         (eq? (current-async-context) context)
                                         (raise-continuable 'probe))))))
                               'migratable? #t)
                             tasks))))])
              (map task-join tasks)))
          'parallelism 4
          'preemption-ticks 1000)
        '((#t 0) (#t 1) (#t 2) (#t 3) (#t 4) (#t 5)
          (#t 6) (#t 7) (#t 8) (#t 9) (#t 10) (#t 11))))

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

  ;; Deep native-fiber stacks preserve every live frame when work is
  ;; published to, and potentially resumed by, another scheduler thread.
  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let* ([n 48]
                   [depth 96]
                   [gate (make-future)]
                   [tasks
                    (let loop ([i 0] [tasks '()])
                      (if (fx= i n)
                          tasks
                          (loop (fx+ i 1)
                            (cons
                              (spawn-task
                                (lambda ()
                                  (future-get gate)
                                  (async-test-deep-yield depth))
                                'migratable? #t)
                              tasks))))])
              (task-yield)
              (future-fulfil! gate)
              (let loop ([tasks tasks] [sum 0])
                (if (null? tasks)
                    sum
                    (loop (cdr tasks)
                      (fx+ sum (task-join (car tasks))))))))
          'parallelism 4)
        (fx* 48 96)))

  ;; Repeated unbuffered rendezvous resumes both fibers beneath the same deep
  ;; live stack.  The protocol alternates exactly, so every round exercises a
  ;; suspension and resumption in each direction.
  (or (not (threaded?))
      (equal?
        (run-async
          (lambda ()
            (let* ([depth 128]
                   [rounds 512]
                   [ping (make-channel)]
                   [pong (make-channel)]
                   [pinger
                    (spawn-task
                      (lambda ()
                        (async-test-call-at-depth depth
                          (lambda ()
                            (let loop ([i 0])
                              (if (fx= i rounds)
                                  i
                                  (begin
                                    (channel-put ping i)
                                    (unless (fx= (channel-get pong) i)
                                      (error 'async-test-ping-pong
                                        "unexpected pong"))
                                    (loop (fx+ i 1))))))))
                      'migratable? #t)]
                   [ponger
                    (spawn-task
                      (lambda ()
                        (async-test-call-at-depth depth
                          (lambda ()
                            (let loop ([i 0])
                              (if (fx= i rounds)
                                  i
                                  (let ([value (channel-get ping)])
                                    (unless (fx= value i)
                                      (error 'async-test-ping-pong
                                        "unexpected ping"))
                                    (channel-put pong value)
                                    (loop (fx+ i 1))))))))
                      'migratable? #t)])
              (list (task-join pinger) (task-join ponger))))
          'parallelism 2)
        '(640 640)))

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
                   [started 0])
              (do ([i 0 (fx+ i 1)]) ((fx= i n))
                (vector-set! tasks i
                  (spawn-task
                    (let ([i i]
                          [context (make-async-context)])
                      (lambda ()
                        (call-with-async-context context
                          (lambda ()
                            (let ([scheduler (current-async-scheduler)])
                              ;; Hold one task on each scheduler until all
                              ;; three scheduler threads have entered user
                              ;; code.
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
                                (task-yield))
                              (vector-set! results i
                                (list (async-scheduler? scheduler)
                                      (eq?
                                        (task-scheduler (current-async-task))
                                        (current-async-scheduler))
                                      (eq? (current-async-context)
                                           context))))))))
                    'migratable? #t)))
              (let loop ()
                (unless (with-mutex mutex (fx= started n))
                  (task-yield)
                  (loop)))
              ;; Completing a future publishes parked tasks to worker deques.
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
                  (fx= (hashtable-size ids) n)
                  (exists
                    (lambda (scheduler)
                      (not (eq? scheduler (vector-ref schedulers 0))))
                    (cdr (vector->list schedulers)))))))
          'parallelism 3)
        '(#t #t #t)))

  ;; Channel delivery can cross scheduler threads and resume a migratable
  ;; native task on any scheduler in the group.
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

  ;; A cancellation can overtake a normal completion after the waiter becomes
  ;; runnable.  Either result may win, and both paths
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

  ;; ---- syntax layer

  (equal?
    (call-with-values (lambda () (async (values 'a 'b))) list)
    '(a b))

  ;; Scheduler option expressions are evaluated once in source order.
  (equal?
    (let ([log '()])
      (async/options
        ([clock (begin (set! log (cons 'clock log)) 'virtual)]
         [preemption-ticks
          (begin (set! log (cons 'preemption-ticks log)) #f)])
        (reverse log)))
    '(clock preemption-ticks))

  ;; go returns a task, await preserves all task values, and go/options
  ;; evaluates option values before spawning the body.
  (equal?
    (async
      (let ([log '()])
        (let ([task
               (go/options
                 ([name (begin (set! log (cons 'name log)) 'worker)]
                  [migratable?
                   (begin (set! log (cons 'migratable? log)) #t)])
                 (values (reverse log) (task-name (current-async-task))))])
          (call-with-values (lambda () (await task)) list))))
    '((name migratable?) worker))

  ;; A join operation composes with choice. Selecting another branch removes
  ;; only the join waiter and leaves the target running.
  (equal?
    (async/options ([clock 'virtual])
      (let ([target (go (async-sleep 2) 'finished)])
        (let ([selected
               (perform-operation
                 (choice-operation
                   (wrap-operation (task-join-operation target)
                     (lambda (value) (list 'joined value)))
                   (always-operation 'other)))])
          (list selected (await target)))))
    '(other finished))
  (error? (task-join-operation 'not-a-task))
  (error?
    (async
      (perform-operation
        (task-join-operation (current-async-task)))))

  ;; General select clauses preserve multiple values and selected branch
  ;; values. select-operation remains a first-class operation.
  (equal?
    (async
      (call-with-values
        (lambda ()
          (perform-operation
            (select-operation
              [(on (always-operation 1 2) left right)
               (values (+ left right) (* left right))])))
        list))
    '(3 2))
  (equal?
    (operation?
      (select-operation
        [(on (always-operation 'ready) value) value]))
    #t)

  ;; Clause inputs are evaluated once in source order, and only the selected
  ;; body runs.
  (equal?
    (async/options ([clock 'virtual])
      (let ([log '()] [channel (make-channel 1)])
        (channel-put channel 'message)
        (let ([answer
               (select
                 [(recv (begin (set! log (cons 'recv log)) channel)
                        value open?)
                  (set! log (cons 'recv-body log))
                  (list value open?)]
                 [(send (begin (set! log (cons 'send-channel log)) channel)
                        (begin (set! log (cons 'send-value log)) 'unused))
                  (set! log (cons 'send-body log))
                  'sent]
                 [(after (begin (set! log (cons 'after log)) 1))
                  (set! log (cons 'after-body log))
                  'late])])
          (list answer (reverse log)))))
    '((message #t) (recv send-channel send-value after recv-body)))

  (equal?
    (call-with-values
      (lambda ()
        (async
          (select
            [(on (never-operation) value) value]
            [else (values 'default 'branch)])))
      list)
    '(default branch))

  ;; send and after shorthands use ordinary channel and timer operations.
  (equal?
    (async/options ([clock 'virtual])
      (let ([channel (make-channel)])
        (let ([receiver (go (channel-get channel))])
          (list
            (select [(send channel 'sent) 'delivered]
                    [(after 1) 'late])
            (await receiver)))))
    '(delivered sent))
  (equal?
    (async/options ([clock 'virtual])
      (select [(after 1) 'timeout]))
    'timeout)

  ;; Dynamic-scope forms preserve values and cancellation behavior.
  (equal?
    (call-with-values
      (lambda ()
        (async
          (let ([context (make-async-context)])
            (with-async-context context
              (values (eq? context (current-async-context)) 'value)))))
      list)
    '(#t value))
  (equal?
    (call-with-values
      (lambda () (async (with-timeout 1 (values 'a 'b))))
      list)
    '(a b))
  (equal?
    (async/options ([clock 'virtual])
      (guard (condition
               [(async-cancellation-condition? condition)
                (async-cancellation-reason condition)])
        (with-cancel-scope (cancel!)
          (cancel! 'stop)
          (async-sleep 1)
          'unreachable)))
    'stop)

  ;; Channel iteration drains buffered values before stopping at closed and
  ;; empty, without closing the channel itself.
  (equal?
    (async
      (let ([channel (make-channel 3)] [values '()])
        (channel-put channel 1)
        (channel-put channel 2)
        (channel-close! channel)
        (channel-for (value channel)
          (set! values (cons value values)))
        (list (reverse values) (channel-closed? channel))))
    '((1 2) #t))

  ;; ---- async mutexes

  (and (async-mutex? (make-async-mutex))
       (not (async-mutex? 'not-a-mutex)))
  (error? (async-mutex-acquire (make-async-mutex)))
  (error? (async-mutex-release! (make-async-mutex)))
  (error? (async-mutex-acquire-operation 'not-a-mutex))
  (error? (call-with-async-mutex (make-async-mutex) 'not-a-procedure))

  ;; Scoped locking preserves multiple values and releases on normal return.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)])
          (let ([first
                 (call-with-values
                   (lambda () (with-async-mutex mutex (values 1 2 3)))
                   list)])
            (async-mutex-acquire mutex)
            (async-mutex-release! mutex)
            first))))
    '(1 2 3))

  ;; A mutex is nonrecursive and can only be released by its owning task.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)])
          (async-mutex-acquire mutex)
          (let ([recursive?
                 (guard (c [else #t])
                   (async-mutex-acquire mutex)
                   #f)]
                [foreign?
                 (task-join
                   (spawn-task
                     (lambda ()
                       (guard (c [else #t])
                         (async-mutex-release! mutex)
                         #f))))])
            (async-mutex-release! mutex)
            (list recursive? foreign?)))))
    '(#t #t))

  ;; Contended acquisitions are granted in registration order.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)] [order '()] [tasks '()])
          (async-mutex-acquire mutex)
          (do ([i 0 (fx+ i 1)]) ((fx= i 4))
            (let ([started (make-future)])
              (set! tasks
                (append tasks
                  (list
                    (spawn-task
                      (let ([i i])
                        (lambda ()
                          (future-fulfil! started #t)
                          (with-async-mutex mutex
                            (set! order (append order (list i))))))))))
              (future-get started)))
          (async-mutex-release! mutex)
          (for-each task-join tasks)
          order)))
    '(0 1 2 3))

  ;; Canceling a blocked acquisition removes it from the wait queue.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)] [started (make-future)])
          (async-mutex-acquire mutex)
          (let ([waiter
                 (spawn-task
                   (lambda ()
                     (future-fulfil! started #t)
                     (async-mutex-acquire mutex)
                     'unexpected))])
            (future-get started)
            (task-cancel! waiter 'stop)
            (let ([result
                   (guard (c [(async-cancellation-condition? c) 'canceled])
                     (task-join waiter))])
              (async-mutex-release! mutex)
              (with-async-mutex mutex (list result 'reacquired)))))))
    '(canceled reacquired))

  ;; A losing acquisition operation withdraws its registered waiter.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)]
              [started (make-future)]
              [ready (make-future)])
          (async-mutex-acquire mutex)
          (let ([waiter
                 (spawn-task
                   (lambda ()
                     (future-fulfil! started #t)
                     (perform-operation
                       (choice-operation
                         (wrap-operation
                           (async-mutex-acquire-operation mutex)
                           (lambda () 'acquired))
                         (future-operation ready)))))])
            (future-get started)
            (future-fulfil! ready 'other)
            (let ([result (task-join waiter)])
              (async-mutex-release! mutex)
              (with-async-mutex mutex (list result 'reacquired)))))))
    '(other reacquired))

  ;; Exceptions release the mutex.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)])
          (guard (c [else (void)])
            (with-async-mutex mutex
              (raise (make-message-condition "leave"))))
          (with-async-mutex mutex 'reacquired))))
    'reacquired)

  ;; Cancellation unwinds a scoped lock before the task terminates.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)] [entered (make-future)])
          (let ([holder
                 (spawn-task
                   (lambda ()
                     (with-async-mutex mutex
                       (future-fulfil! entered #t)
                       (async-sleep 100))))])
            (future-get entered)
            (task-cancel! holder 'stop)
            (guard (c [(async-cancellation-condition? c) (void)])
              (task-join holder))
            (with-async-mutex mutex 'reacquired)))))
    'reacquired)

  ;; Task termination releases an unscoped acquisition as a final backstop.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)])
          (task-join
            (spawn-task
              (lambda ()
                (async-mutex-acquire mutex)
                'finished)))
          (with-async-mutex mutex 'reacquired))))
    'reacquired)

  ;; The owned-lock index remains exact after it grows beyond the short-list
  ;; representation used by ordinary tasks.
  (equal?
    (run-async
      (lambda ()
        (let ([mutexes
               (let loop ([i 0] [mutexes '()])
                 (if (fx= i 12)
                     mutexes
                     (loop (fx+ i 1)
                       (cons (make-async-mutex) mutexes))))])
          (task-join
            (spawn-task
              (lambda ()
                (for-each async-mutex-acquire mutexes)
                'finished)))
          (for-each async-mutex-acquire mutexes)
          (for-each async-mutex-release! mutexes)
          'reacquired)))
    'reacquired)

  ;; Suspension and scheduler migration retain ownership until scope exit.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)]
              [entered (make-future)]
              [release (make-future)]
              [order '()])
          (let ([holder
                 (spawn-task
                   (lambda ()
                     (with-async-mutex mutex
                       (set! order (cons 'holder-enter order))
                       (future-fulfil! entered #t)
                       (future-get release)
                       (set! order (cons 'holder-exit order))))
                   'migratable? #t)])
            (future-get entered)
            (let ([waiter
                   (spawn-task
                     (lambda ()
                       (with-async-mutex mutex
                         (set! order (cons 'waiter order))))
                     'migratable? #t)])
              (task-yield)
              (future-fulfil! release #t)
              (task-join holder)
              (task-join waiter)
              (reverse order)))))
      'parallelism 3)
    '(holder-enter holder-exit waiter))

  ;; Native fiber preemption does not expose a held mutex to another worker.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)]
              [entered (make-future)]
              [inside? #f])
          (let ([holder
                 (spawn-task
                   (lambda ()
                     (with-async-mutex mutex
                       (set! inside? #t)
                       (future-fulfil! entered #t)
                       (async-test-burn 100000)
                       (set! inside? #f)))
                   'migratable? #t)])
            (future-get entered)
            (let ([waiter
                   (spawn-task
                     (lambda ()
                       (with-async-mutex mutex inside?))
                     'migratable? #t)])
              (task-join holder)
              (task-join waiter)))))
      'parallelism 3
      'preemption-ticks 1000)
    #f)

  ;; The mutex serializes migratable tasks across worker schedulers.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-mutex)] [counter 0] [tasks '()])
          (do ([i 0 (fx+ i 1)]) ((fx= i 8))
            (set! tasks
              (cons
                (spawn-task
                  (lambda ()
                    (do ([j 0 (fx+ j 1)]) ((fx= j 40))
                      (with-async-mutex mutex
                        (let ([old counter])
                          (task-yield)
                          (set! counter (fx+ old 1))))))
                  'migratable? #t)
                tasks)))
          (for-each task-join tasks)
          counter))
      'parallelism 4)
    320)

  ;; Simultaneous choice handoffs cannot corrupt a task's owned-lock set.
  (equal?
    (run-async
      (lambda ()
        (do ([iteration 0 (fx+ iteration 1)])
            ((fx= iteration 25) 'clean)
          (let ([left (make-async-mutex)]
                [right (make-async-mutex)]
                [owners-ready (make-async-wait-group 2)]
                [release (make-future)])
            (define make-owner
              (lambda (mutex)
                (spawn-task
                  (lambda ()
                    (with-async-mutex mutex
                      (async-wait-group-done! owners-ready)
                      (future-get release)))
                  'migratable? #t)))
            (let ([left-owner (make-owner left)]
                  [right-owner (make-owner right)])
              (async-wait-group-wait owners-ready)
              (let ([chooser
                     (spawn-task
                       (lambda ()
                         (perform-operation
                           (choice-operation
                             (wrap-operation
                               (async-mutex-acquire-operation left)
                               (lambda () 'left))
                             (wrap-operation
                               (async-mutex-acquire-operation right)
                               (lambda () 'right)))))
                       'migratable? #t)])
                (let wait-until-registered ()
                  (unless (task-current-wait chooser)
                    (task-yield)
                    (wait-until-registered)))
                (future-fulfil! release #t)
                (task-join left-owner)
                (task-join right-owner)
                ;; The chooser deliberately leaves its winning acquisition
                ;; unscoped; termination cleanup must release it.
                (task-join chooser)
                (with-async-mutex left (void))
                (with-async-mutex right (void)))))))
      'parallelism 4)
    'clean)

  ;; ---- read/write mutexes

  (and (async-rw-mutex? (make-async-rw-mutex))
       (not (async-rw-mutex? 'not-a-mutex)))
  (error? (async-rw-mutex-acquire (make-async-rw-mutex)))
  (error? (async-rw-mutex-read-acquire 'not-a-mutex))

  ;; Multiple readers may overlap across schedulers.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-rw-mutex)]
              [first-entered (make-future)]
              [second-entered (make-future)]
              [release (make-future)])
          (let ([first
                 (spawn-task
                   (lambda ()
                     (with-async-read-mutex mutex
                       (future-fulfil! first-entered #t)
                       (future-get release)
                       'first))
                   'migratable? #t)])
            (future-get first-entered)
            (let ([second
                   (spawn-task
                     (lambda ()
                       (with-async-read-mutex mutex
                         (future-fulfil! second-entered #t)
                         'second))
                     'migratable? #t)])
              (future-get second-entered)
              (future-fulfil! release #t)
              (list (task-join first) (task-join second))))))
      'parallelism 3)
    '(first second))

  ;; A queued writer precedes readers that arrive later.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-rw-mutex)] [order '()])
          (async-rw-mutex-read-acquire mutex)
          (let ([writer-started (make-future)])
            (let ([writer
                   (spawn-task
                     (lambda ()
                       (future-fulfil! writer-started #t)
                       (with-async-rw-mutex mutex
                         (set! order (append order '(writer))))))])
              (future-get writer-started)
              (let ([reader-started (make-future)])
                (let ([reader
                       (spawn-task
                         (lambda ()
                           (future-fulfil! reader-started #t)
                           (with-async-read-mutex mutex
                             (set! order (append order '(reader))))))])
                  (future-get reader-started)
                  (async-rw-mutex-read-release! mutex)
                  (task-join writer)
                  (task-join reader)
                  order)))))))
    '(writer reader))

  ;; Read locks are recursive; write locks and upgrades are not.
  (equal?
    (run-async
      (lambda ()
        (let ([mutex (make-async-rw-mutex)] [writer-ran? #f])
          (async-rw-mutex-read-acquire mutex)
          (async-rw-mutex-read-acquire mutex)
          (let ([upgrade?
                 (guard (c [else #t])
                   (async-rw-mutex-acquire mutex)
                   #f)])
            (async-rw-mutex-read-release! mutex)
            (let ([writer
                   (spawn-task
                     (lambda ()
                       (with-async-rw-mutex mutex
                         (set! writer-ran? #t))))])
              (task-yield)
              (let ([blocked? (not writer-ran?)])
                (async-rw-mutex-read-release! mutex)
                (task-join writer)
                (async-rw-mutex-acquire mutex)
                (let ([recursive?
                       (guard (c [else #t])
                         (async-rw-mutex-acquire mutex)
                         #f)])
                  (async-rw-mutex-release! mutex)
                  (list upgrade? blocked? recursive? writer-ran?))))))))
    '(#t #t #t #t))

  ;; ---- wait groups

  (and (async-wait-group? (make-async-wait-group))
       (not (async-wait-group? 'not-a-group)))
  (error? (make-async-wait-group -1))
  (error? (async-wait-group-done! (make-async-wait-group)))
  (error? (spawn-task/async-wait-group
            (make-async-wait-group) 'not-a-procedure))

  ;; A spawn validation failure rolls back membership before propagating.
  (equal?
    (run-async
      (lambda ()
        (let ([group (make-async-wait-group)])
          (guard (c [else (void)])
            (spawn-task/async-wait-group group (lambda () (void))
              'unknown-option #t))
          (async-wait-group-wait group)
          'rolled-back)))
    'rolled-back)

  ;; Scoped spawning adds before publication, preserves task values, and
  ;; balances the counter when the child exits.
  (equal?
    (run-async
      (lambda ()
        (let ([group (make-async-wait-group)])
          (let ([task
                 (with-async-wait-group group
                   (values 1 2 3))])
            (async-wait-group-wait group)
            (call-with-values (lambda () (task-join task)) list)))))
    '(1 2 3))

  ;; Exceptions and cancellation also leave scoped membership.
  (equal?
    (run-async
      (lambda ()
        (let ([group (make-async-wait-group)] [started (make-future)])
          (let ([failed
                 (with-async-wait-group group
                   (raise (make-message-condition "worker failed")))])
            (guard (c [else (void)]) (task-join failed)))
          (let ([worker
                 (with-async-wait-group group
                   (future-fulfil! started #t)
                   (async-sleep 10))])
            (future-get started)
            (task-cancel! worker 'stop)
            (guard (c [else (void)]) (task-join worker))
            (async-wait-group-wait group)
            'balanced))))
    'balanced)

  ;; Cancellation before the child's first turn still balances membership.
  (equal?
    (run-async
      (lambda ()
        (let ([group (make-async-wait-group)] [ran? #f])
          (let ([worker
                 (with-async-wait-group group
                   (set! ran? #t))])
            (task-cancel! worker 'before-start)
            (guard (c [else (void)]) (task-join worker))
            (async-wait-group-wait group)
            (list ran? 'balanced)))))
    '(#f balanced))

  ;; The completion action belongs only to the registered task.  A nested
  ;; ordinary spawn must not inherit it and release the group early.
  (equal?
    (run-async
      (lambda ()
        (let ([group (make-async-wait-group)] [outer-finished? #f])
          (with-async-wait-group group
            (task-join (spawn-task (lambda () (void))))
            (async-sleep 0.02)
            (set! outer-finished? #t))
          (async-wait-group-wait group)
          outer-finished?)))
    #t)

  (equal?
    (run-async
      (lambda ()
        (let ([group (make-async-wait-group 2)] [order '()])
          (spawn-task
            (lambda ()
              (task-yield)
              (set! order (cons 'first order))
              (async-wait-group-done! group)))
          (spawn-task
            (lambda ()
              (task-yield)
              (task-yield)
              (set! order (cons 'second order))
              (async-wait-group-done! group)))
          (async-wait-group-wait group)
          ;; A completed group can be reused after its wait returns.
          (async-wait-group-add! group 1)
          (spawn-task (lambda () (async-wait-group-done! group)))
          (perform-operation (async-wait-group-wait-operation group))
          (reverse order))))
    '(first second))

  ;; Cancellation withdraws a wait-group operation without changing count.
  (equal?
    (run-async
      (lambda ()
        (let ([group (make-async-wait-group 1)] [started (make-future)])
          (let ([waiter
                 (spawn-task
                   (lambda ()
                     (future-fulfil! started #t)
                     (async-wait-group-wait group)))])
            (future-get started)
            (task-cancel! waiter 'stop)
            (guard (c [else (void)]) (task-join waiter))
            (async-wait-group-done! group)
            (async-wait-group-wait group)
            'usable))))
    'usable)

  ;; Wait groups accept exact counters larger than the fixnum range.
  (equal?
    (run-async
      (lambda ()
        (let* ([count (expt 2 80)]
               [group (make-async-wait-group count)])
          (async-wait-group-add! group (- count))
          (async-wait-group-wait group)
          'large-counter)))
    'large-counter)

  ;; ---- once

  (and (async-once? (make-async-once))
       (not (async-once? 'not-once)))
  (error? (async-once-run! (make-async-once) (lambda () (void))))

  (equal?
    (run-async
      (lambda ()
        (let ([once (make-async-once)] [count 0] [tasks '()])
          (do ([i 0 (fx+ i 1)]) ((fx= i 12))
            (set! tasks
              (cons
                (spawn-task
                  (lambda ()
                    (async-once-run! once
                      (lambda ()
                        (task-yield)
                        (set! count (fx+ count 1)))))
                  'migratable? #t)
                tasks)))
          (for-each task-join tasks)
          count))
      'parallelism 4)
    1)

  ;; An exception marks Once as done; later invocations do not retry it.
  (equal?
    (run-async
      (lambda ()
        (let ([once (make-async-once)] [count 0])
          (guard (c [else (void)])
            (async-once-run! once
              (lambda ()
                (set! count 1)
                (raise (make-message-condition "once failed")))))
          (async-once-run! once (lambda () (set! count 2)))
          count)))
    1)

  ;; Cancellation of the initializer completes Once and releases its waiters.
  (equal?
    (run-async
      (lambda ()
        (let ([once (make-async-once)] [started (make-future)])
          (let ([initializer
                 (spawn-task
                   (lambda ()
                     (async-once-run! once
                       (lambda ()
                         (future-fulfil! started #t)
                         (async-sleep 10)))))])
            (future-get started)
            (let ([waiter
                   (spawn-task
                     (lambda ()
                       (async-once-run! once (lambda () 'not-run))
                       'released))])
              (task-yield)
              (task-cancel! initializer 'stop)
              (guard (c [else (void)]) (task-join initializer))
              (let ([result (task-join waiter)])
                (async-once-run! once (lambda () 'not-run))
                result)))))
      'parallelism 2
      'preemption-ticks 1000)
    'released)

  ;; ---- conditions

  (let* ([mutex (make-async-mutex)]
         [condition (make-async-condition mutex)])
    (and (async-condition? condition)
         (eq? (async-condition-mutex condition) mutex)
         (not (async-condition? 'not-a-condition))))
  (error?
    (run-async
      (lambda ()
        (async-condition-wait
          (make-async-condition (make-async-mutex))))))

  ;; Wait atomically registers, releases, suspends, and reacquires the mutex.
  (equal?
    (run-async
      (lambda ()
        (let* ([mutex (make-async-mutex)]
               [condition (make-async-condition mutex)]
               [started (make-future)]
               [ready? #f]
               [waiter
                (spawn-task
                  (lambda ()
                    (with-async-mutex mutex
                      (future-fulfil! started #t)
                      (let loop ()
                        (unless ready?
                          (async-condition-wait condition)
                          (loop)))
                      'awakened)))])
          (future-get started)
          (with-async-mutex mutex
            (set! ready? #t)
            (async-condition-signal! condition))
          (task-join waiter))))
    'awakened)

  ;; Broadcast wakes every waiter, and an RWMutex can be the condition lock.
  (equal?
    (run-async
      (lambda ()
        (let* ([mutex (make-async-rw-mutex)]
               [condition (make-async-condition mutex)]
               [started (make-async-wait-group 3)]
               [ready? #f]
               [tasks
                (map
                  (lambda (id)
                    (spawn-task
                      (lambda ()
                        (with-async-rw-mutex mutex
                          (async-wait-group-done! started)
                          (let loop ()
                            (unless ready?
                              (async-condition-wait condition)
                              (loop)))
                          id))))
                  '(a b c))])
          (async-wait-group-wait started)
          (with-async-rw-mutex mutex
            (set! ready? #t)
            (async-condition-broadcast! condition))
          (map task-join tasks))))
    '(a b c))

  ;; A canceled condition waiter reacquires before unwinding its lock scope.
  (equal?
    (run-async
      (lambda ()
        (let* ([mutex (make-async-mutex)]
               [condition (make-async-condition mutex)]
               [started (make-future)]
               [waiter
                (spawn-task
                  (lambda ()
                    (with-async-mutex mutex
                      (future-fulfil! started #t)
                      (async-condition-wait condition))))])
          (future-get started)
          (task-cancel! waiter 'stop)
          (guard (c [else (void)]) (task-join waiter))
          (with-async-mutex mutex 'reacquired))))
    'reacquired)

  ;; Cancellation already pending at Wait leaves the still-owned mutex alone.
  (equal?
    (run-async
      (lambda ()
        (let* ([mutex (make-async-mutex)]
               [condition (make-async-condition mutex)]
               [observed #f]
               [waiter
                (spawn-task
                  (lambda ()
                    (with-async-mutex mutex
                      (task-cancel! (current-async-task) 'stop)
                      (set! observed
                        (guard (c
                                 [(async-cancellation-condition? c)
                                  'canceled]
                                 [else 'wrong-condition])
                          (async-condition-wait condition)
                          'unexpected)))))])
          (guard (c [else (void)]) (task-join waiter))
          (list observed (with-async-mutex mutex 'usable)))))
    '(canceled usable))

  ;; ---- observability

  (let ([saved-task #f] [saved-scheduler #f] [saved-cost-center #f]
        [parked-stack #f])
    (run-async
      (lambda ()
        (set! saved-task (current-async-task))
        (set! saved-scheduler (current-async-scheduler))
        (set! saved-cost-center (make-cost-center 'async-observer))
        (with-cost-center #t saved-cost-center
          (lambda ()
            (let ([sleeper (spawn-task (lambda () (async-sleep 1)))])
              (let wait ()
                (unless (eq? (task-state sleeper) 'waiting)
                  (task-yield)
                  (wait)))
              (set! parked-stack (task-stack-snapshot sleeper))
              (task-cancel! sleeper 'observed)
              (guard (c [else (void)]) (task-join sleeper))
              (task-yield)))))
      'parallelism 2
      'trace? #t)
    (let ([task-data (task-profile saved-task)]
          [scheduler-data (async-scheduler-profile saved-scheduler)]
          [events (async-scheduler-trace saved-scheduler)])
      (and (pair? parked-stack)
           (> (cdr (assq 'run-count task-data)) 0)
           (>= (cdr (assq 'runtime-us task-data)) 0)
           (>= (cdr (assq 'wait-time-us task-data)) 0)
           (>= (cdr (assq 'queue-time-us task-data)) 0)
           (>= (cdr (assq 'migration-count task-data)) 0)
           (> (cdr (assq 'execution-count scheduler-data)) 0)
           (>= (cdr (assq 'preemption-count scheduler-data)) 0)
           (time>=? (cost-center-time saved-cost-center)
             (make-time 'time-duration 0 0))
           (pair? events)
           (ormap (lambda (event) (eq? (vector-ref event 2) 'finish)) events)
           (begin
             (async-scheduler-trace-clear! saved-scheduler)
             (null? (async-scheduler-trace saved-scheduler))))))

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
                  (and (pair? w) (eq? (car w) 'choice))))))
    #t)
)
