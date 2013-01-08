(define (double f)
  (lambda (x)
    (f (f x))))

(define inc (lambda (x) (+ x 1)))

(double inc)

;; produces 21
;; (+ (expt 2 (expt 2 2)) 5)
(((double (double double)) inc) 5)
;; (+ (expt 2 3) 5)
((double (double (double inc))) 5)
