#! /usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 receive)
             (ice-9 format)
             (ice-9 regex))

(define letter-num-alist
  '((A . 10)
    (B . 11)
    (C . 12)
    (D . 13)
    (E . 14)
    (F . 15)
    (G . 16)
    (H . 17)
    (I . 34)
    (J . 18)
    (K . 19)
    (M . 21)
    (N . 22)
    (O . 35)
    (P . 23)
    (Q . 24)
    (T . 27)
    (U . 28)
    (V . 29)
    (W . 32)
    (X . 30)
    (Z . 33)))

(define (split-serial x)
  (let R ([ret '()] [x1 x] [digits 7])
    (if (= 0 digits)
        ret
        (if (= 0 x1)
            (R (cons 0 ret) 0 (- digits 1))
            (receive (head tail)
                (truncate/ x1 10)
              (R (cons tail ret) head (- digits 1)))))))

(define (serial-checksum x)
  (let R ([x1 (split-serial x)] [mtpy 7] [sum 0])
    (if (null? x1)
        sum
        (R (cdr x1) (- mtpy 1) (+ sum (* (car x1) mtpy))))))

(define (mixed-checksum anum d2 serial)
  (receive (d0 d1)
      (truncate/ anum 10)
    (+ d0 (* 9 d1) (* 8 d2) (serial-checksum serial))))

(define (last-digit x)
  (remainder (- 10 (remainder x 10)) 10))

(define (generate-and-print area sex serial)
  (let ((area1 (if (null? area)
                   (list-ref letter-num-alist (random (length letter-num-alist)))
                   area))
        (sex1 (if (null? sex)
                  (+ 1 (random 2))
                  sex))
        (serial1 (if (null? serial)
                     (random 10000000)
                     serial)))
    (format #t "~a~d~7,'0d~d~%"
            (symbol->string (car area1))
            sex1
            serial1
            (last-digit (mixed-checksum (cdr area1) sex1 serial1)))))

(define (valid-letter x)
  (if (string-match (format #f "[~a]" (map car letter-num-alist)) x)
      x
      #f))

(define (validate x)
    (if (and
         (valid-letter (substring x 0 1))
         (string-match "[0-9]{9}" (substring x 1)))
        (let ([area (assq-ref letter-num-alist (string->symbol (substring x 0 1)))]
              [sex (string->number (substring x 1 2))]
              [serial (string->number (substring x 2 9))])
          (if (= (string->number (substring x 9)) (last-digit (mixed-checksum area sex serial)))
              #t
              #f))
        #f))

(define (main args)
  (let ([alen (length args)])
    (if (and (= alen 3) (string= (list-ref args 1) "-c"))
        (if (validate (list-ref args 2))
            (format #t "good~%")
            (format #t "bad~%"))
        (begin
          (set! *random-state* (random-state-from-platform))
          (generate-and-print (if (>= alen 2)
                                  (assq (string->symbol (list-ref args 1)) letter-num-alist)
                                  '())
                              (if (>= alen 3)
                                  (string->number (list-ref args 2))
                                  '())
                              (if (>= alen 4)
                                  (string->number (list-ref args 3))
                                  '()))))))
