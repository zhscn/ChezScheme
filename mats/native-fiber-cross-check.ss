;;; Driver for compiling native-fiber-cross-probe.ss with a cross patch.

(define (bytevector-contains? haystack needle)
  (let ([haystack-length (bytevector-length haystack)]
        [needle-length (bytevector-length needle)])
    (let outer ([start 0])
      (and (fx<= (fx+ start needle-length) haystack-length)
           (let inner ([offset 0])
             (cond
               [(fx= offset needle-length) #t]
               [(fx= (bytevector-u8-ref haystack (fx+ start offset))
                     (bytevector-u8-ref needle offset))
                (inner (fx+ offset 1))]
               [else (outer (fx+ start 1))]))))))

(define (target-fence-sequence machine)
  (case machine
    [(trv64le)
     ;; fence r,rw; fence rw,w
     #vu8(#x0f #x00 #x30 #x02 #x0f #x00 #x10 #x03)]
    [(tarm64le tarm64osx tarm64nt)
     ;; dmb ishld; dmb ish
     #vu8(#xbf #x39 #x03 #xd5 #xbf #x3b #x03 #xd5)]
    [else
     (error 'native-fiber-cross-check
       "unsupported cross-check target" machine)]))

(let ([args (command-line)])
  (unless (fx= (length args) 6)
    (error 'native-fiber-cross-check
      "expected target, xpatch, input, output, and assembly paths" args))
  (let ([target (string->symbol (list-ref args 1))]
        [xpatch (list-ref args 2)]
        [input (list-ref args 3)]
        [output (list-ref args 4)]
        [assembly (list-ref args 5)])
    (load xpatch)
    (let ([assembly-port (open-output-file assembly '(replace))])
      (dynamic-wind
        void
        (lambda ()
          (parameterize ([#%$assembly-output assembly-port])
            (compile-file input output)))
        (lambda () (close-output-port assembly-port)))
      (let ([image
             (call-with-port
               (open-file-input-port output)
               get-bytevector-all)]
            [expected (target-fence-sequence target)])
        (unless (bytevector-contains? image expected)
          (error 'native-fiber-cross-check
            "target acquire/release fence encoding is absent" target))))))
