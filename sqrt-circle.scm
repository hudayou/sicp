(define (average x y)
  (/ (+ x y) 2.0))

(define (new-if predicate then-clause else-clause)
  ;;(cond (predicate then-clause)
  ;;      (else else-clause)))
  then-clause)
        ;;(else (else-clause))))

(define (my-sqrt y)
  (define (good-enough? x)
    ;;(< (abs (- y (* x x))) 1e-9))
    #t)
  (define (improve x)
    (average x (/ y x)))
  (define (try x)
    ;; in the definition of a procedure the tail call can't be
    ;; a procedure calling itself; otherwise, the applicative order
    ;; evaluation will use up the stack. f(x) = f(f(x))
    (display "trying ")
    (display x)
    (display "\n")
    ;;(new-if (lambda () (good-enough? x))
    (new-if (good-enough? x)
    ;;(if (good-enough? x)
            x
            ;;(lambda () (try (improve x)))))
            (try (improve x))))
  (try 1.0))

;; when good-enough? finally evaluates to #t
;;(try 1.0)
;;(new-if #f 1.0 (try 2.0))
;;(new-if #f 2.0 (try 1.75))
;;(new-if #f 1.73214285714286 (try 1.73205081001473))
;;(new-if #f 1.73205081001473 (try 1.73205080756888))
;;(new-if #t 1.73205080756888 (try 1.73205080756888))

;; when good-enough? finally evaluates to #f
;;(try 1.0)
;;(new-if #f 1.0 (try 2.0))
;;(new-if #f 2.0 (try 1.75))
;;(new-if #f 1.73214285714286 (try 1.73205081001473))
;;(new-if #f 1.73205081001473 (try 1.73205080756888))
;;(new-if #f 1.73205080756888 (try 1.73205080756888))
