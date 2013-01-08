(use-modules (sicp utils))

(define dx 1e-9)

(define (smooth f)
  (lambda (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))
