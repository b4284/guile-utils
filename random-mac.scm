(use-modules (ice-9 format))

(define (main args)
  (with-input-from-file "/dev/urandom"
    (lambda ()
      (let ([p (current-input-port)])
        (let six ([i 6])
          (if (> i 0)
              (let ([byte (string-upcase (format #f "~2,'0x" (char->integer (read-char p))))])
                (display byte)
                (if (> i 1)
                    (display ":")
                    (newline))
                (six (- i 1)))))))))
