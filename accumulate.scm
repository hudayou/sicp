;; iterative
(define (iter-accumulate combiner null-value term a next b)
  (define (iter j r)
    (if (> j b)
      r
      (iter (next j) (combiner (term j) r))))
  (iter a null-value))

;; recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
       (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))
