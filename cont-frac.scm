;; iterative process
(define (iter-cont-frac n d k)
  (define (iter i r)
    (if (< i 1)
      r
      (iter (- i 1) (/ (n i) (+ (d i) r)))))
  (iter k 0))

;; recursive process
(define (cont-frac n d k)
  (define (cont j)
    (if (> j k)
      0
      (/ (n j) (+ (d k) (cont (+ j 1))))))
  (if (< k 1)
    0
    (cont 1)))

;; (/ (1- (sqrt 5)) 2)
;; k must be at least 11 to be able to accurate to 4 decimal places

;; (= e (exp 1))
;; (- e 2)
(define (e k)
  (+ 2
     (iter-cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (let ((j
                          (remainder i 3))
                        (k
                          (quotient i 3)))
                    (cond ((= j 2) (+ 2.0 (* 2 k)))
                          (else 1.0))))
                k)))
