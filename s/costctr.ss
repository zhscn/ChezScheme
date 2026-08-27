;;; costctr.ss
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

(let ()
  (if-feature pthreads
    (define-record-type ($cost-center $make-cost-center $cost-center?)
      (fields
        (mutable level)
        (mutable instr-count)
        (mutable alloc-count)
        (mutable time-ns)
        (mutable time-s)
        (immutable mutex)
        (immutable name))
      (nongenerative #{cost-center k3k8qugwirxkqxmvxdy2jz0xm-1})
      (opaque #t)
      (protocol
        (lambda (new)
          (lambda (name)
            (new (make-thread-parameter 0) 0 0 0 0 (make-mutex) name)))))
    (define-record-type ($cost-center $make-cost-center $cost-center?)
      (fields
        (mutable level)
        (mutable instr-count)
        (mutable alloc-count)
        (mutable time-ns)
        (mutable time-s)
        (immutable name))
      (nongenerative #{cost-center k3k8qugwirxkqxmvxdy2jz0xm-2})
      (opaque #t)
      (protocol
        (lambda (new)
          (lambda (name) (new 0 0 0 0 0 name))))))

  (define-syntax cc-level
    (lambda (x)
      (syntax-case x ()
        [(_ x)
         (if-feature pthreads
           #'(($cost-center-level x))
           #'($cost-center-level x))])))

  (define-syntax cc-level-set!
    (lambda (x)
      (syntax-case x ()
        [(_ x v)
         (if-feature pthreads
           #'(($cost-center-level x) v)
           #'($cost-center-level-set! x v))])))

  (define-syntax with-cost-center-mutex
    (lambda (x)
      (syntax-case x ()
        [(_ cc e0 e1 ...)
         (if-feature pthreads
           #'(with-mutex ($cost-center-mutex cc) e0 e1 ...)
           #'(begin e0 e1 ...))])))

  (define-record-type fiber-cost-frame
    (nongenerative fiber-cost-frame-layer1)
    (sealed #t)
    (fields
      (immutable cost-center)
      (immutable timed?)
      (mutable counting?)
      (mutable active?)
      (mutable alloc)
      (mutable instr)
      (mutable time)))

  (define-record-type saved
    (sealed #t)
    (nongenerative cost-center-saved-layer1)
    (fields (mutable alloc) (mutable intr) (mutable time)))

  (define fiber-cost-table (make-weak-eq-hashtable))
  (define fiber-cost-active-count (box 0))
  (define fiber-cost-table-mutex
    (if-feature pthreads (make-mutex) #f))

  (define-syntax with-fiber-cost-table
    (lambda (x)
      (syntax-case x ()
        [(_ e0 e1 ...)
         (if-feature pthreads
           #'(with-mutex fiber-cost-table-mutex e0 e1 ...)
           #'(begin e0 e1 ...))])))

  (define counter-mod-
    (lambda (x y)
      (let ([r (- x y)])
        (if (< r 0) (+ (expt 2 64) r) r))))

  (define add-frame-cost!
    (lambda (frame curr-alloc curr-instr curr-time)
      (let* ([cc (fiber-cost-frame-cost-center frame)]
             [alloc-count (counter-mod- curr-alloc (fiber-cost-frame-alloc frame))]
             [instr-count (counter-mod- curr-instr (fiber-cost-frame-instr frame))])
        (with-cost-center-mutex cc
          ($cost-center-alloc-count-set! cc
            (+ ($cost-center-alloc-count cc) alloc-count))
          ($cost-center-instr-count-set! cc
            (+ ($cost-center-instr-count cc) instr-count))
          (when (fiber-cost-frame-timed? frame)
            (let* ([saved-time (fiber-cost-frame-time frame)]
                   [ns (- (time-nanosecond curr-time)
                          (time-nanosecond saved-time))]
                   [s (- (time-second curr-time)
                         (time-second saved-time))]
                   [s (if (< ns 0) (- s 1) s)]
                   [ns (if (< ns 0) (+ ns (expt 10 9)) ns)]
                   [ns (+ ($cost-center-time-ns cc) ns)]
                   [s (+ ($cost-center-time-s cc) s)]
                   [s (if (>= ns (expt 10 9)) (+ s 1) s)]
                   [ns (if (>= ns (expt 10 9)) (- ns (expt 10 9)) ns)])
              ($cost-center-time-s-set! cc s)
              ($cost-center-time-ns-set! cc ns)))))))

  (define fiber-frame-start!
    (lambda (frame)
      (when (and (fiber-cost-frame-counting? frame)
                 (not (fiber-cost-frame-active? frame)))
        (fiber-cost-frame-alloc-set! frame
          ($object-ref 'unsigned-64 ($tc) (constant tc-alloc-counter-disp)))
        (fiber-cost-frame-instr-set! frame
          ($object-ref 'unsigned-64 ($tc) (constant tc-instr-counter-disp)))
        (when (fiber-cost-frame-timed? frame)
          (fiber-cost-frame-time-set! frame (current-time 'time-thread)))
        (fiber-cost-frame-active?-set! frame #t))))

  (define fiber-frame-stop!
    (lambda (frame)
      (when (fiber-cost-frame-active? frame)
        ;; Read time first to exclude as much accounting overhead as possible.
        (let ([curr-time (and (fiber-cost-frame-timed? frame)
                              (current-time 'time-thread))]
              [curr-alloc ($object-ref 'unsigned-64 ($tc)
                            (constant tc-alloc-counter-disp))]
              [curr-instr ($object-ref 'unsigned-64 ($tc)
                            (constant tc-instr-counter-disp))])
          (fiber-cost-frame-active?-set! frame #f)
          (add-frame-cost! frame curr-alloc curr-instr curr-time)))))

  (define fiber-frames
    (lambda (fiber)
      (if (eqv? ($atomic-box-ref fiber-cost-active-count) 0)
          '()
          (with-fiber-cost-table
            (hashtable-ref fiber-cost-table fiber '())))))

  (define atomic-count-add!
    (lambda (box delta)
      (let loop ([old ($atomic-box-ref box)])
        (unless (box-cas! box old (+ old delta))
          (loop ($atomic-box-ref box))))))

  (define with-fiber-cost-center
    (lambda (fiber timed? cc th)
      (let ([frame (make-fiber-cost-frame cc timed? #f #f 0 0 #f)])
        (dynamic-wind
          (lambda ()
            (with-fiber-cost-table
              (let ([frames (hashtable-ref fiber-cost-table fiber '())])
                (fiber-cost-frame-counting?-set! frame
                  (not (ormap
                         (lambda (other)
                           (and (fiber-cost-frame-counting? other)
                                (eq? (fiber-cost-frame-cost-center other) cc)))
                         frames)))
                (hashtable-set! fiber-cost-table fiber (cons frame frames))
                (atomic-count-add! fiber-cost-active-count 1)))
            (fiber-frame-start! frame))
          th
          (lambda ()
            (fiber-frame-stop! frame)
            (with-fiber-cost-table
              (let ([frames (remq frame
                              (hashtable-ref fiber-cost-table fiber '()))])
                (if (null? frames)
                    (hashtable-delete! fiber-cost-table fiber)
                    (hashtable-set! fiber-cost-table fiber frames))
                (atomic-count-add! fiber-cost-active-count -1))))))))

  (define $with-cost-center
    (let ()
      (define who 'with-cost-center)
      (define-syntax with-mutex-if-threaded
        (lambda (x)
          (syntax-case x ()
            [(_ mexp e0 e1 ...)
             (if-feature pthreads
               #'(with-mutex mexp e0 e1 ...)
               #'(begin e0 e1 ...))])))
      (lambda (timed? cc th)
        (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
        (unless (procedure? th) ($oops who "~s is not a procedure" th))
        (let ([fiber ($current-native-fiber)])
          (if fiber
              (with-fiber-cost-center fiber timed? cc th)
              (let ([saved (make-saved 0 0 #f)])
          (dynamic-wind #t
            (lambda ()
              (let ([level (cc-level cc)])
                (cc-level-set! cc (fx+ level 1))
                (when (fx= level 0)
                  (saved-alloc-set! saved ($object-ref 'unsigned-64 ($tc) (constant tc-alloc-counter-disp)))
                  (saved-intr-set! saved ($object-ref 'unsigned-64 ($tc) (constant tc-instr-counter-disp)))
                  (when timed? (saved-time-set! saved (current-time 'time-thread))))))
            th
            (lambda ()
              (let ([level (cc-level cc)])
                (cc-level-set! cc (fx- level 1))
                (when (fx= level 1)
                  ; grab time first -- to use up as little as possible
                  (let* ([curr-time (and timed? (current-time 'time-thread))]
                         [alloc-count (counter-mod- ($object-ref 'unsigned-64 ($tc) (constant tc-alloc-counter-disp))
                                        (saved-alloc saved))]
                         [instr-count (counter-mod- ($object-ref 'unsigned-64 ($tc) (constant tc-instr-counter-disp))
                                        (saved-intr saved))])
                    (with-mutex-if-threaded ($cost-center-mutex cc)
                      ($cost-center-alloc-count-set! cc
                        (+ ($cost-center-alloc-count cc) alloc-count))
                      ($cost-center-instr-count-set! cc
                        (+ ($cost-center-instr-count cc) instr-count))
                      (when timed?
                        (let ([saved-time (saved-time saved)])
                          (let-values ([(s ns) (let ([ns (- (time-nanosecond curr-time) (time-nanosecond saved-time))]
                                                     [s (- (time-second curr-time) (time-second saved-time))])
                                                 (if (< ns 0)
                                                     (values (- s 1) (+ ns (expt 10 9)))
                                                     (values s ns)))])
                            (let-values ([(s ns)  (let ([ns (+ ($cost-center-time-ns cc) ns)]
                                                        [s (+ ($cost-center-time-s cc) s)])
                                                    (if (>= ns (expt 10 9))
                                                        (values (+ s 1) (- ns (expt 10 9)))
                                                        (values s ns)))])
                              ($cost-center-time-s-set! cc s)
                              ($cost-center-time-ns-set! cc ns)))))))))))))))))

  (set! $cost-center-fiber-switch-out!
    (lambda (fiber)
      (for-each fiber-frame-stop! (fiber-frames fiber))))

  (set! $cost-center-fiber-switch-in!
    (lambda (fiber)
      (for-each fiber-frame-start! (fiber-frames fiber))))

  (set-who! cost-center-instruction-count
    (lambda (cc)
      (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
      ($cost-center-instr-count cc)))

  (set-who! cost-center-allocation-count
    (lambda (cc)
      (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
      (ash ($cost-center-alloc-count cc) (constant log2-ptr-bytes))))

  (set-who! cost-center-name
    (lambda (cc)
      (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
      ($cost-center-name cc)))

  (set-who! cost-center-time
    (lambda (cc)
      (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
      (make-time 'time-duration ($cost-center-time-ns cc) ($cost-center-time-s cc))))
  
  (set-who! reset-cost-center!
    (lambda (cc)
      (unless ($cost-center? cc) ($oops who "~s is not a cost center" cc))
      ($cost-center-instr-count-set! cc 0)
      ($cost-center-alloc-count-set! cc 0)
      ($cost-center-time-s-set! cc 0)
      ($cost-center-time-ns-set! cc 0)))

  (set! cost-center? (lambda (x) ($cost-center? x)))

  (set-who! make-cost-center
            (case-lambda
              [() ($make-cost-center #f)]
              [(name)
               (unless (or (not name) (symbol? name))
                 ($oops who "~s is not a symbol or #f" name))
               ($make-cost-center name)]))

  (set! with-cost-center
    (rec with-cost-center
      (case-lambda
        [(cc th) ($with-cost-center #f cc th)]
        [(timed? cc th) ($with-cost-center timed? cc th)])))

  (record-writer (record-type-descriptor $cost-center)
    (lambda (x p wr)
      (let ([ns ($cost-center-time-ns x)] [s ($cost-center-time-s x)]
            [name ($cost-center-name x)])
        (fprintf p "#<cost center~[~*~:; ~a~]~[~2*~:; t=~d.~9,'0d~]~[~:; i=~:*~s~]~[~:; a=~:*~s~]>"
          (if name 1 0) name
          (+ ns s) s ns
          ($cost-center-instr-count x)
          ($cost-center-alloc-count x))))))
