(define (approx x y)
  (/ (+ (/ y (* x x)) (* 2.0 x)) 3.0))

(define (cubert y)
  (define (good-enough? x)
    (< (abs (- y (* x x x))) 1e-13))
  (define (improve x)
    (approx x y))
  (define (try x)
    (if (good-enough? x)
      x
      (try (improve x))))
  (try 1.0))
