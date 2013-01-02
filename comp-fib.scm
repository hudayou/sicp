;; f(n) = n for n < 3
;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) for n >= 3

;; linear recursive process
(define (comp-fib n)
  (define (multi m)
    (* m (comp-fib (- n m))))
  (if (< n 3)
    n
    (+ (multi 1) (multi 2) (multi 3))))

;; iterative process
;; a <- f(2)
;; b <- f(1)
;; c <- f(0)
;; a <- a + 2b + 3c
;; b <- a
;; c <- b
(define (iter-comp-fib n)
  (define (iter a b c count)
    (if (= count 0)
      a
      (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (if (< n 3)
    n
    (iter 2 1 0 (- n 2))))
