;; A(x, y) = A(x-1, A(x, y-1))
;; A(0, y) = 2y
;; A(x, 0) = 0
;; A(x, 1) = 2

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
