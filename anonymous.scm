(define (f g)
  (g 2))

(define square (lambda (x) (* x x)))

(f square)

(f (lambda (z) (* z (+ z 1))))

;; (f f) -> (f (f 2)) -> (f (2 2)) -> misc error
