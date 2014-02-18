(define (colname->number str)
  (let R ([y (reverse (string->list (string-upcase str)))] [sum 0] [index 0])
    (if (null? y)
        sum
        (let ([char (* (expt 26 index) (- (char->integer (car y)) 64))])
          (R (cdr y) (+ sum char) (+ 1 index))))))

(define (main args)
  (display (colname->number (list-ref args 1)))
  (newline))
