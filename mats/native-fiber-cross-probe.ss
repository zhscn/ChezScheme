;;; Compile-only coverage for target-specific native-fiber and foreign-call
;;; lowering.  The companion cross-check driver also verifies the encoded
;;; acquire/release fence pair in this file.

;; Keep the fence probe as a bare expression. Its code bytes are stored
;; directly in the compiled file, which lets the cross-check distinguish the
;; RISC-V predecessor/successor masks instead of merely seeing `fence` in the
;; compiler's textual assembly output.
(lambda ()
  (#3%memory-order-acquire)
  (#3%memory-order-release))

(define (native-fiber-cross-fence-probe)
  (#3%memory-order-acquire)
  (#3%memory-order-release))

(define (native-fiber-cross-switch-probe scheduler task payload)
  (and (#3%$native-fiber-try-claim! task)
       (#3%$native-fiber-switch scheduler task payload)))

(define (native-fiber-cross-ffi-probe)
  (let ([call-callback
         (foreign-procedure "call_for_interrupt_test" (void* int) int)]
        [call-i64
         (foreign-procedure
           "call_i64" (ptr integer-64 int int) integer-64)]
        [call-df
         (foreign-procedure
           "call_df" (ptr double-float int int) double-float)]
        [call-many
         (foreign-procedure "call_with_many_args" (void*) void)]
        [block
         (foreign-procedure __collect_safe
           "collect_safe_block_until_release" (int) int)])
    (let ([inner
           (foreign-callable (lambda (x) (fx+ x 1)) (int) int)]
          [i64-callback
           (foreign-callable
             (lambda (x) (+ x 5))
             (integer-64) integer-64)]
          [df-callback
           (foreign-callable
             (lambda (x) (+ x 5.0))
             (double-float) double-float)]
          [many-callback
           (foreign-callable
             (lambda (i s1 s2 s3 s4 i2 s6 s7 i3)
               (vector i s1 s2 s3 s4 i2 s6 s7 i3)
               (void))
             (int u8* u8* u8* u8* int u8* u8* int)
             void)])
      (let ([r1 (call-callback (foreign-callable-entry-point inner) 40)]
            [r2 (call-i64 i64-callback 1099511627776 7 23)]
            [r3 (call-df df-callback 73.25 7 23)]
            [r4 (block 1)])
        (call-many (foreign-callable-entry-point many-callback))
        (keep-live inner)
        (keep-live i64-callback)
        (keep-live df-callback)
        (keep-live many-callback)
        (vector r1 r2 r3 r4)))))
