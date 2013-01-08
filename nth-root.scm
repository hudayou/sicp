(use-modules (sicp utils))

;; cheat here
(define (nth-root x n less-than-n)
  (fixed-point
    ;; need to understand the difference of ((repeated square 2) 5) and
    ;; (repeated (square 5) 2), the first one repeat sqaure twice on 5
    ((repeated average-damp less-than-n)
     (lambda (y) (/ x (fexpt y (- n 1)))))
     ;;(floor (log2 n)))
     1.0))
