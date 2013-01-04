;; linear recursive process
(define (multi a b)
  (if (= b 0)
    0
    (+ a (multi a (- b 1)))))

;; iterative process
;; no * operator is allowed
(define (iter-multi a b)
  (define (double x) (+ x x))
  (define (halve x)
    ;; (= #t (even? x))
    (ash x -1))
  (define (iter a b c)
    ;; invariant quantity (+ (* a b) c)
    (cond ((zero? b) c)
          ((odd? b) (iter a (1- b) (+ c a)))
          ((even? b) (iter (double a) (halve b) c))))
  (iter a b 0))
