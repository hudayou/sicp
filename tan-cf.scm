;; iterative process
(define (iter-cont-frac combiner null-value n d k)
  (define (iter i r)
    (if (< i 1)
      r
      (iter (- i 1) (/ (n i) (combiner (d i) r)))))
  (iter k null-value))

;; compute (tan x)
(define (tan-cf x k)
  (iter-cont-frac
    -
    0
    (lambda (i)
      (cond ((= i 1) x)
            (else (* x x))))
    (lambda (i)
      (- (* 2 i) 1.0))
    k))
