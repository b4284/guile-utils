#! /usr/bin/guile \
-e main --debug -s
!#

(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 format)
             (ice-9 receive))

(define (read-file-to-list x)
  (with-input-from-file x
    (lambda ()
      (let R ([content '()] [line (read-line (current-input-port))])
        (if (eof-object? line)
            (reverse content)
            (R (cons line content) (read-line (current-input-port))))))))

(define (pickup-these-lines x)
  (if (string-match "(redirect|forward|render).*\\(.*\\)" x)
      #t
      #f))

(define (action-begin x d)
  (let R ([lines x] [depth d] [accu-lines '()])
    (if (= 0 depth)
        (values accu-lines lines)
        (let ([line (car lines)])
          (R (cdr lines) (+ depth (line-depth line)) (if (pickup-these-lines line)
                                                  (cons line accu-lines)
                                                  accu-lines))))))

(define (line-depth x)
  (let ([count1 (string-count x #\{)]
        [count2 (string-count x #\})])
    (- count1 count2)))

(define (analyse-pickup c x)
  (let ([match1 (string-match "render.*(template|view): *['\"]([^'\"]*)['\"]" x)])
    (if match1
        (let ([match2 (match:substring match1 2)])
          (string-append (match:substring match1 1) ":"
                         (if (string-index match2 #\/)
                             match2
                             (string-append "/" c "/" match2))))
        (let ([match3 (string-match "(forward|redirect).*(controller: *['\"]?([^'\" ,]*)['\"]?)?.*action: *['\"]?([^'\" ,]*)['\"]?" x)])
          (if match3
              (string-append c "/" (match:substring match3 4))
              #f)))))

(define (analyse-action c x)
  (let ([action-name (string-trim-both (match:substring (string-match "def(.*)=.*\\{" (car x)) 1))])
    (let R1 ([pickups (cdr x)] [results '()])
      (if (null? pickups)
          (cons action-name (filter (lambda (x) (if x #t #f)) results))
          (let ([anal (analyse-pickup c (car pickups))])
            (if (member anal results)
                (R1 (cdr pickups) results)
                (R1 (cdr pickups) (cons anal results))))))))

(define (uncapitalize x)
  (string-append (string-downcase (substring x 0 1)) (substring x 1)))

(define (controller-name x)
  (let ([match (string-match "class (.*)\\{" (car x))])
    (if match
        (let* ([name1 (string-trim-both (match:substring match 1))]
               [name2 (uncapitalize (substring name1 0 (- (string-length name1) 10)))])
          name2)
        (controller-name (cdr x)))))

(define (print-results-as-dot c x)
  (print-dot-cluster c x)
  (print-dot-relationship c x))

(define (print-dot-cluster c x)
  (format #t "~a {~%" (string-append "subgraph cluster" (string-capitalize c) "Controller"))
  (let R ([ls x])
    (if (not (null? ls))
        (let ([action-name (car (car ls))]
              [pickups (cdr (car ls))])
          (format #t "    \"~a/~a\"[label=~a];~%" c action-name action-name)
          (let R2 ([pickups1 pickups])
            (if (not (null? pickups1))
                (begin
                  (if (string-contains (car pickups1) "/ec110/")
                      (format #t "    \"~a\";~%" (car pickups1)))
                  (R2 (cdr pickups1)))
                (R (cdr ls)))))))
  (format #t "}~%"))

(define (print-dot-relationship c x)
  (if (not (null? x))
      (let ([action-name (car (car x))])
        (let R ([pickups (cdr (car x))])
          (if (null? pickups)
              (print-dot-relationship c (cdr x))
              (begin
                (format #t "\"~a/~a\" -> \"~a\";~%" c action-name (car pickups))
                (R (cdr pickups))))))))

(define (main args)
  (if (= 2 (length args))
      (let ([content-list (read-file-to-list (list-ref args 1))])
        (let ([ctrller-name (controller-name content-list)])
          (let R1 ([lines content-list] [actions '()])
            (if (not (null? lines))
                (let ([line (car lines)])
                  (if (string-match "def.*=.*\\{" line)
                      (receive (action-pickups after-lines)
                          (action-begin (cdr lines) (line-depth line))
                        (R1 (cdr after-lines) (cons (cons line action-pickups) actions)))
                      (R1 (cdr lines) actions)))
                (let R2 ([a2 actions] [results '()])
                  (if (null? a2)
                      (print-results-as-dot ctrller-name results)
                      (R2 (cdr a2) (cons (analyse-action ctrller-name (car a2)) results))))))))))
