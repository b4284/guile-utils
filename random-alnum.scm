#!/usr/bin/guile -s
!#

(define LENGTH (if (> (length (command-line)) 1)
                   (let ((res (string->number (list-ref (command-line) 1))))
                     (if (and res (integer? res)) res (error "Must be an integer.")))
                   16))

(set! *random-state* (random-state-from-platform))

(let A ([ret '()])
  (if (= (length ret) LENGTH)
      (begin
        (display (list->string (reverse ret)))
        (newline))
      (let* ([num (random 62)]
             [num1 (+ 48 num)]
             [num2 (+ num1 (if (>= num 10) 7 0))]
             [num3 (+ num2 (if (>= num 36) 6 0))]
             [char (integer->char num3)])
        (A (cons char ret)))))
