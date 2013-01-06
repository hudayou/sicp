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

(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter j r)
    (define (comb)
      (if (predicate j)
        (combiner (term j) r)
        r))
    (if (> j b)
      r
      (iter (next j) (comb))))
  (iter a null-value))

(define (square  x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ;;(else (find-divisor n (+ test-divisor 1)))))
        ;; next statetment is two times faster since input grows two
        ;; times faster
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

;; the sum of the squares of the prime numbers in the interval a to b
(filtered-accumulate + 0 square 1 1+ 5 (lambda (x) (prime? x)))

;; the product of all the positive integers less than n that are relatively
;; prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).
(filtered-accumulate * 1 (lambda (x) x) 1 1+ 5
                     (lambda (x) (= (gcd x 5) 1)))
