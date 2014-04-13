#!/usr/bin/guile \
-e main -s
!#

(use-modules (sxml simple))

(define (convert sxml)
  (let R ([pw-list (cdaddr (cdaddr sxml))]
          [converted '()])
    (if (null? pw-list)
        `(database (group (title Internet) (icon 1) ,converted))
        (let* ([content (cdar pw-list)]
               [title (car content)]
               [user (cadr content)]
               [url (caddr content)]
               [pw (cadddr content)]
               [rest (cddddr content)])
          (set-car! user 'username)
          (if (null? (cdr url))
              (set-cdr! url '(url)))
          (R (cdr pw-list) (cons `(entry ,title ,user ,url ,pw) converted))))))

(define (main args)
  (let* ([sxml (with-input-from-file (list-ref args 1)
           (lambda () (xml->sxml (current-input-port))))]
         [converted (convert sxml)])
    (display "<!DOCTYPE KEEPASSX_DATABASE>")
    (newline)
    (display (sxml->xml converted))
    (newline)))

