;; (display (list 'a 'b 'c))
;; (newline)
;; (display (list (list 'george)))
;; (newline)
;; (display (cdr '((x1 x2) (y1 y2))))
;; (newline)
;; (display (cadr '((x1 x2) (y1 y2))))
;; (newline)
;; (display (pair? (car '(a short list))))
;; (newline)
;; (display (memq 'red '((red shoes) (blue socks))))
;; (newline)
;; (display (memq 'red '(red shoes blue socks)))
;; (newline)

(define (equal? x y)
  (if (and (pair? x)
           (pair? y))
    (and (equal? (car x)
                 (car y))
         (equal? (cdr x)
                 (cdr y)))
    (eq? x y)))

;; prints quote
;; since the interpreter will interpret it as
;; '(quote abracadabra) or
;; (car (quote (quote abracadabra)))
(car ''abracadabra)
