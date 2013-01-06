(define (sum term a next b)
  (define (iter j r)
    (if (> j b)
      r
      (iter (next j) (+ (term j) r))))
  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-rule f a b n)
  (define (check?)
    ;; return #t if n passes check
    ;; otherwise #f
    (and (integer? n) (positive? n) (even? n) (<= a b)))
  (define h (/ (- b a) n))
  (define (iter i r)
    (define next
      ;; n and 0 are even
      (cond ((odd? i) (* 4 (f (+ a (* h i)))))
            ((= i 0) (f a))
            ((= i n) (f (+ a (* h i))))
            ((even? i) (* 2 (f (+ a (* h i)))))))
    (cond ((> i n) r)
          (else (iter (+ i 1) (+ r next)))))
  (if (check?)
    (* (/ h 3.0) (iter 0 0))
    #f))

(define (cube x) (* x x x))

;; result is (* (/ 1 4.0) (expt x 4))
(simpson-rule cube 0 1 100)

(simpson-rule cube 0 1 1000)
