(use-modules (ice-9 rdelim))

(let ((s (socket PF_INET SOCK_STREAM 0)))
  (connect s AF_INET (inet-pton AF_INET "127.0.0.1") 8080)
  (display "GET /mosw/auth/login HTTP/1.0\r\n\r\n" s)

  (do ((line (read-line s) (read-line s)))
      ((eof-object? line))
    (display line)
    (newline)))

(addrinfo:addr (car (getaddrinfo "10.1.2.199" "6667")))

(let ((s (socket PF_INET SOCK_STREAM 0)))
  (connect s (addrinfo:addr (car (getaddrinfo "10.1.2.199" "6667"))))
  ;; (display "GET / HTTP/1.0\r\n\r\n" s)
  (do ((line (read-line s) (read-line s)))
      ((not (char-ready? s)))
    (display line)
    (newline))
  (close s))
  ;; (connect s (addrinfo:addr ai)))

(do ((i 2 (+ i 1)))
    ((= i 3))
  (display "a")
    )
