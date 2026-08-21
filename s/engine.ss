;;; engine.ss
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

;;; Notes:
;;; The engine code defines three functions: make-engine,
;;; engine-block, and engine-return.

;;; Keyboard interrupts are caught while an engine is running
;;; and the engine disabled while the handler is running.

;;; All of the engine code is defined within local state
;;; containing the following variables:
;;;   *active*  true iff an engine is running
;;;   *exit*    the continuation to the engine invoker
;;;   *keybd*   the saved keyboard interrupt handler
;;;   *timer*   the saved timer interrupt handler


(let ()

(define-threaded *exit*)
(define-threaded *keybd*)
(define-threaded *timer*)
(define-threaded *active* #f)
(define-threaded *block-hook* void)
(define-threaded *resume-hook* void)

(define cleanup
  (lambda (who)
    (unless *active* ($oops who "no engine active"))
    (set! *active* #f)
    (keyboard-interrupt-handler *keybd*)
    (timer-interrupt-handler *timer*)
    (set! *keybd* (void))
    (set! *exit* (void))
    (set! *timer* (void))
    (set! *block-hook* void)
    (set! *resume-hook* void)))

(define setup
  (lambda (exit)
    (set! *active* #t)
    (set! *keybd* (keyboard-interrupt-handler))
    (keyboard-interrupt-handler (exception *keybd*))
    (set! *timer* (timer-interrupt-handler))
    (timer-interrupt-handler block)
    (set! *exit* exit)))

(define block
 ; disable engine and return the continuation
  (lambda ()
    (let ([exit *exit*]
          [block-hook *block-hook*])
      (block-hook)
      (cleanup 'engine-block)
      (set-timer (call/cc (lambda (k) (exit (lambda () k)))))
      (*resume-hook*))))

(define return
 ; disable engine and return list (ticks value ...)
  (lambda (args)
    (let ([n (set-timer 0)])
      (let ([exit *exit*])
        (cleanup 'engine-return)
        (exit (lambda () (cons n args)))))))

(define exception
 ; disable engine while calling the handler
  (lambda (handler)
    (lambda args
      (let ([ticks (set-timer 0)]
            [block-hook *block-hook*]
            [resume-hook *resume-hook*])
        (let ([exit *exit*])
          (cleanup 'engine-exception)
          (apply handler args)
          (set! *block-hook* block-hook)
          (set! *resume-hook* resume-hook)
          (setup exit)
          (if (= ticks 0) (block) (set-timer ticks)))))))

(define run-engine
 ; run a continuation as an engine
  (lambda (k ticks block-hook resume-hook)
    ((call/cc
       (lambda (exit)
         (set-timer 0)
         (when *active* ($oops 'engine "cannot nest engines"))
         (set! *block-hook* block-hook)
         (set! *resume-hook* resume-hook)
         (setup exit)
         (k ticks))))))

(define eng
 ; create an engine from a procedure or continuation
  (lambda (k block-hook resume-hook)
    (lambda (ticks complete expire)
      (unless (and (fixnum? ticks) (not (negative? ticks)))
        ($oops 'engine "invalid ticks ~s" ticks))
      (unless (procedure? complete)
        ($oops 'engine "~s is not a procedure" complete))
      (unless (procedure? expire)
        ($oops 'engine "~s is not a procedure" expire))
      (if (= ticks 0)
          (expire (eng k block-hook resume-hook))
          (let ([x (run-engine k ticks block-hook resume-hook)])
            (if (procedure? x)
                (expire (eng x block-hook resume-hook))
                (apply complete x)))))))

(define make-engine*
  (lambda (x block-hook resume-hook)
    (eng (lambda (ticks) 
           (with-exception-handler
             (lambda (c)
               (let ([ticks (set-timer 0)]
                     [block-hook *block-hook*]
                     [resume-hook *resume-hook*])
                 (let ([exit *exit*])
                   (cleanup 'raise)
                   (call/cc
                     (lambda (k)
                       (exit
                         (lambda ()
                           (let-values ([vals (raise-continuable c)])
                             (set! *block-hook* block-hook)
                             (set! *resume-hook* resume-hook)
                             (setup exit)
                             (if (= ticks 0) (block) (set-timer ticks))
                             (apply k vals)))))))))
             (lambda ()
               (set-timer ticks)
               (call-with-values x (lambda args (return args))))))
         block-hook
         resume-hook)))

(set! engine-return (lambda args (return args)))

(set! engine-block (lambda () (set-timer 0) (block)))

(set! make-engine
  (lambda (x)
    (unless (procedure? x) ($oops 'make-engine "~s is not a procedure" x))
    (make-engine* x void void)))

(set! $make-engine-with-timer-hooks
  (lambda (x block-hook resume-hook)
    (unless (procedure? x)
      ($oops '$make-engine-with-timer-hooks "~s is not a procedure" x))
    (unless (procedure? block-hook)
      ($oops '$make-engine-with-timer-hooks "~s is not a procedure" block-hook))
    (unless (procedure? resume-hook)
      ($oops '$make-engine-with-timer-hooks "~s is not a procedure" resume-hook))
    (make-engine* x block-hook resume-hook)))

;;; Fiber schedulers copy the thread-parameter vector to isolate dynamic
;;; state.  Engine bookkeeping is runtime state, not user dynamic state, so a
;;; copied snapshot must be cleared before it is installed for another task.
(set! $engine-reset-thread-state!
  (lambda ()
    (set! *active* #f)
    (set! *exit* (void))
    (set! *keybd* (void))
    (set! *timer* (void))
    (set! *block-hook* void)
    (set! *resume-hook* void)))

(set! $engine-active? (lambda () *active*))
)
