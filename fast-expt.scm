(define (square x) (* x x))

;; recursive process
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b(/ n 2))))
        ((odd? n) (* b (fast-expt b (- n 1))))))

;; iterative process
(define (iter-fexpt b n)
  (define (iter a b n)
    ;; invariant quantity is (* a (expt b n))
    (if (= n 0)
      a
      (cond ((odd? n) (iter (* a b) b (1- n)))
            ((even? n) (iter a (square b) (/ n 2))))))
  (iter 1 b n))
