;; (define (log3 x) (/ (log x) (log 3)))
;; T(n) = T(3/n) + O(1)
;; space :
;; (- (log3 angle) (log3 limit))
;; time :
;; (* (log3 angle) c)
;; c refers to the execution time of p
(define (cube x) (* x x x ))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define counter 0)
(define limit 1e-9)
(define (sine angle)
  (if (not (> (abs angle) limit))
    angle
    (begin
      (set! counter (1+ counter))
      (p (sine (/ angle 3.0))))))
