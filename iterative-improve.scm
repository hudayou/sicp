(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
          next
          (try next))))
    (try first-guess)))

(define (fixed-point f first-guess)
  (define tolerance 1e-9)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(define (my-sqrt x)
  (define tolerance 1e-9)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (average x y)
    (/ (+ x y) 2.0))
  (define (improve g)
    (average g (/ x g)))
  ((iterative-improve close-enough? improve) 1.0))
