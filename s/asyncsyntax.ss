;;; asyncsyntax.ss
;;; Copyright 2026 Cisco Systems, Inc.
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

(define-syntax async-syntax:async
  (syntax-rules ()
    [(_ body1 body2 ...)
     (run-async (lambda () body1 body2 ...))]))

(define-syntax async-syntax:async/options
  (lambda (x)
    (define parse-options
      (lambda (option* allowed who)
        (let loop ([option* option*] [seen '()] [binding* '()] [argument* '()])
          (if (null? option*)
              (values (reverse binding*) (reverse argument*))
              (syntax-case (car option*) ()
                [(key value)
                 (identifier? #'key)
                 (let ([key (syntax->datum #'key)])
                   (unless (memq key allowed)
                     (syntax-violation who "unrecognized option" x #'key))
                   (when (memq key seen)
                     (syntax-violation who "duplicate option" x #'key))
                   (with-syntax ([temporary
                                  (car (generate-temporaries #'(value)))])
                     (loop (cdr option*) (cons key seen)
                       (cons #'[temporary value] binding*)
                       (cons #'temporary
                         (cons #`(quote #,(datum->syntax #'key key))
                           argument*)))))]
                [_ (syntax-violation who "invalid option" x (car option*))])))))
    (syntax-case x ()
      [(_ (option ...) body1 body2 ...)
       (let-values ([(binding* argument*)
                     (parse-options (syntax->list #'(option ...))
                       '(clock parallelism preemption-ticks)
                       'async/options)])
         (with-syntax ([([temporary value] ...) binding*]
                       [(argument ...) argument*])
           #'(let* ([temporary value] ...)
               (run-async (lambda () body1 body2 ...) argument ...))))])))

(define-syntax async-syntax:go
  (syntax-rules ()
    [(_ body1 body2 ...)
     (spawn-task (lambda () body1 body2 ...) 'migratable? #t)]))

(define-syntax async-syntax:go/options
  (lambda (x)
    (define parse-options
      (lambda (option*)
        (let loop ([option* option*] [seen '()] [binding* '()] [argument* '()])
          (if (null? option*)
              (values (reverse binding*) (reverse argument*) seen)
              (syntax-case (car option*) ()
                [(key value)
                 (identifier? #'key)
                 (let ([key (syntax->datum #'key)])
                   (unless (memq key '(name group context migratable?))
                     (syntax-violation 'go/options "unrecognized option" x #'key))
                   (when (memq key seen)
                     (syntax-violation 'go/options "duplicate option" x #'key))
                   (with-syntax ([temporary
                                  (car (generate-temporaries #'(value)))])
                     (loop (cdr option*) (cons key seen)
                       (cons #'[temporary value] binding*)
                       (cons #'temporary
                         (cons #`(quote #,(datum->syntax #'key key))
                           argument*)))))]
                [_ (syntax-violation 'go/options "invalid option" x (car option*))])))))
    (syntax-case x ()
      [(_ (option ...) body1 body2 ...)
       (let-values ([(binding* argument* seen)
                     (parse-options (syntax->list #'(option ...)))])
         (let ([argument*
                (if (memq 'migratable? seen)
                    argument*
                    (append argument* (list #'(quote migratable?) #'#t)))])
           (with-syntax ([([temporary value] ...) binding*]
                         [(argument ...) argument*])
             #'(let* ([temporary value] ...)
                 (spawn-task (lambda () body1 body2 ...) argument ...)))))])))

(define-syntax async-syntax:await
  (syntax-rules ()
    [(_ task) (task-join task)]))

(define-syntax async-syntax:select-operation
  (lambda (x)
    (define parse-clauses
      (lambda (clause*)
        (let loop ([clause* clause*] [binding* '()] [operation* '()])
          (if (null? clause*)
              (values binding* operation*)
              (let ([clause (car clause*)])
                (syntax-case clause ()
                  [((on operation variable ...) body1 body2 ...)
                   (and (eq? (syntax->datum #'on) 'on)
                        (andmap identifier? (syntax->list #'(variable ...))))
                   (with-syntax ([temporary
                                  (car (generate-temporaries #'(operation)))])
                     (loop (cdr clause*)
                       (append binding* (list #'[temporary operation]))
                       (append operation*
                         (list
                           #'(wrap-operation temporary
                               (lambda (variable ...) body1 body2 ...))))))]
                  [((recv channel value open?) body1 body2 ...)
                   (and (eq? (syntax->datum #'recv) 'recv)
                        (identifier? #'value) (identifier? #'open?))
                   (with-syntax ([(temporary)
                                  (generate-temporaries #'(channel))])
                     (loop (cdr clause*)
                       (append binding* (list #'[temporary channel]))
                       (append operation*
                         (list
                           #'(wrap-operation
                               (channel-receive-operation temporary)
                               (lambda (value open?) body1 body2 ...))))))]
                  [((send channel value) body1 body2 ...)
                   (eq? (syntax->datum #'send) 'send)
                   (with-syntax ([(channel-tmp value-tmp)
                                  (generate-temporaries #'(channel value))])
                     (loop (cdr clause*)
                       (append binding*
                         (list #'[channel-tmp channel] #'[value-tmp value]))
                       (append operation*
                         (list
                           #'(wrap-operation
                               (channel-put-operation channel-tmp value-tmp)
                               (lambda () body1 body2 ...))))))]
                  [((after seconds) body1 body2 ...)
                   (eq? (syntax->datum #'after) 'after)
                   (with-syntax ([(temporary)
                                  (generate-temporaries #'(seconds))])
                     (loop (cdr clause*)
                       (append binding* (list #'[temporary seconds]))
                       (append operation*
                         (list
                           #'(wrap-operation (sleep-operation temporary)
                               (lambda () body1 body2 ...))))))]
                  [(else body1 body2 ...)
                   (eq? (syntax->datum #'else) 'else)
                   (begin
                     (unless (null? (cdr clause*))
                       (syntax-violation 'select-operation
                         "else clause must be last" x clause))
                     (values binding*
                       (append operation*
                         (list
                           #'(wrap-operation (always-operation)
                               (lambda () body1 body2 ...))))))]
                  [_ (syntax-violation 'select-operation
                       "invalid clause" x clause)]))))))
    (syntax-case x ()
      [(_ clause ...)
       (let-values ([(binding* operation*)
                     (parse-clauses (syntax->list #'(clause ...)))])
         (with-syntax ([([temporary expression] ...) binding*]
                       [(operation ...) operation*])
           #'(let* ([temporary expression] ...)
               (choice-operation operation ...))))])))

(define-syntax async-syntax:select
  (syntax-rules ()
    [(_ clause ...)
     (perform-operation (async-syntax:select-operation clause ...))]))

(define-syntax async-syntax:with-timeout
  (syntax-rules ()
    [(_ seconds body1 body2 ...)
     (call-with-async-timeout seconds (lambda () body1 body2 ...))]))

(define-syntax async-syntax:with-async-context
  (syntax-rules ()
    [(_ context body1 body2 ...)
     (call-with-async-context context (lambda () body1 body2 ...))]))

(define-syntax async-syntax:with-cancel-scope
  (lambda (x)
    (syntax-case x ()
      [(_ (cancel!) body1 body2 ...)
       (identifier? #'cancel!)
       #'(let ([context (make-async-context)])
           (let ([cancel!
                  (case-lambda
                    [() (async-context-cancel! context)]
                    [(reason) (async-context-cancel! context reason)])])
             (async-dynamic-wind
               (lambda () (void))
               (lambda ()
                 (call-with-async-context context
                   (lambda () body1 body2 ...)))
               (lambda ()
                 (async-context-cancel! context 'scope-exited)))))])))

(define-syntax async-syntax:channel-for
  (lambda (x)
    (syntax-case x ()
      [(_ (value channel) body1 body2 ...)
       (identifier? #'value)
       #'(let ([ch channel])
           (let loop ()
             (let-values ([(value open?) (channel-receive ch)])
               (when open?
                 body1 body2 ...
                 (loop)))))])))
