;; exponential fibonacci
(define (slow-fib n)
  (if (< n 2)
    n
    (+ (slow-fib (1- n)) (slow-fib (- n 2)))))

;; iterative fibonacci
(define (iter-fib n)
  (define (iter a b n)
    (if (= n 0)
      b
      (iter (+ a b) a (1- n))))
  (if (< n 2)
    n
    (iter 1 0 n)))

;; logarithmic fibonacci
(define (fib n)
  (fib-iter 1 0 0 1 n))

;; a' <- bq + aq + ap
;; b' <- bp + aq
;; a'' <- b'q + a'q + a'p
;;     <- q(bp + aq) + q(bq + aq + ap) + p(bq + aq + ap)
;;     <- b(pq + qq + pq) + a(pq + qq + pq) + a(pp + qq)
;; b'' <- p(bp + aq) + q(bq + aq + ap)
;;     <- b(pp + qq) + a(pq + qq + pq)
;; p'  <- pp + qq
;; q'  <- pq + pq + qq <- q(p + p +q)
;;
;; (a b) <- (1 0)
;; (p q) <- (0 1)

(define (fib-iter a b p q count)
  (cond ((zero? count) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (* q (+ p p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (1- count)))))
