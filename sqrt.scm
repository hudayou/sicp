(define (average x y)
  (/ (+ x y) 2.0))

(define (my-sqrt y)
  (define (good-enough? x)
    (< (abs (- y (* x x))) 1e-9))
  (define (improve x)
    (average x (/ y x)))
  (define (try x)
    (if (good-enough? x)
      x
      (try (improve x))))
  (try 1.0))
