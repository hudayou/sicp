(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess approx)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        (begin 
          (append approx next)
          (display approx)
          (newline)
          next)
        (try next (append approx (list next))))))
  (try first-guess '()))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

;; golden ratio x -> 1 + 1/x
(fixed-point (lambda (x) (1+ (/ 1 x))) 1.0)

;; 8 times if an average damping is used
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
;; 33 times if no average damping is used
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

