;; pascal triangle
;;
;;              1
;;             1 1
;;            1 2 1
;;           1 3 3 1
;;          1 4 6 4 1

;; p(1) = (1)
;; p(2) = (1 1)
;; q(n) = (append '(0) p(n))
;; r(n) = (append q(n) '(0))
;; add(a b) = (+ a b)
;;
;; t(n) = append p(n) to t(n-1)
;;
;; take n = 2 as example, row(2) will map add to (0 1) and (1 0)
;; which evals to (1 1)
;;
;; linear recusive process
(define (pascal-triangle n)
  (define (add a b)
    (+  a b))
  (define (row m)
    (cond ((< m 1) '())
          ((= m 1) '(1))
          (else 
            (map-in-order
              add
              (append '(0) (row (- m 1)))
              (append (row (- m 1)) '(0))))))
  (cond ((< n 1) '())
        (else (append
                (pascal-triangle (- n 1))
                (list (row n))))))

;; iterative process
(define (iterative-pascal-triangle n)
  ;; a <- triangle(0)
  ;; b <- row(1)
  ;; a <- (append a (list b))
  ;; b <- row(2)
  (define (iter a b n)
    (if (<= n 0)
      a
      (iter 
        (append a (list b))
        (map-in-order
          (lambda (x y)
            (+ x y))
          (append '(0) b)
          (append b '(0)))
        (- n 1))))
  (iter '() '(1) n))
