;; recursive
(define (product a b f next)
  (if (> a b)
    1
    (* (f a)
       (product (next a) b f next))))

;; iterative
(define (iter-product a b f next)
  (define (iter j r)
    (if (> j b)
      r
      (iter (next j) (* r (f j)))))
  (iter a 1))


(define factorial
  (lambda (x)
    (define f (lambda (x) x))
    (product 1 x f 1+)))

(define (pi x)
  (define (f x)
    (cond ((odd? x) (/ (* 2 (/ (1+ x) 2.0)) (1+ (* 2 (/ (1+ x) 2.0)))))
          ((even? x) (/ (* 2 (1+ (/ x 2.0))) (1- (* 2 (1+ (/ x 2.0))))))))
  (* 4 (product 1 x f 1+)))

(define iter-factorial
  (lambda (x)
    (define f (lambda (x) x))
    (iter-product 1 x f 1+)))

(define (iter-pi x)
  (define (f x)
    (cond ((odd? x) (/ (* 2 (/ (1+ x) 2.0)) (1+ (* 2 (/ (1+ x) 2.0)))))
          ((even? x) (/ (* 2 (1+ (/ x 2.0))) (1- (* 2 (1+ (/ x 2.0))))))))
  (* (iter-product 1 x f 1+) 4))
