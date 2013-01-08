(use-modules (sicp utils))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

;; (x2 - 2x + 1) (x - 1) x3 - 3x2 + 3x - 1
(newtons-method (cubic 3 3 1) 1)
