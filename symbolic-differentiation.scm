(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (let ((e (exponent exp))
               (b (base exp)))
           (make-product
             e
             (make-product
               (make-exponentiation
                 b
                 (make-sub e 1))
               (deriv b var)))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (sub? x)
  (and (pair? x) (eq? (car x) '-)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(use-modules (ice-9 pretty-print))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-sub s1 s2)
  (cond ((=number? s1 0) (list '- s2))
        ((=number? s2 0) s1)
        ((and (number? s1) (number? s2)) (- s1 s2))
        (else (list '- s1 s2))))

(define (make-exponentiation b e)
  (cond ((or (=number? e 0) (=number? b 1)) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(pretty-print (deriv '(+ x 3) 'x))
(pretty-print (deriv '(* x y) 'x))
(pretty-print (deriv '(* (* x y) (+ x 3)) 'x))
(pretty-print (deriv '(** u n) 'u))
(pretty-print (deriv '(** u 0) 'u))
(pretty-print (deriv '(** u 1) 'u))
(pretty-print (deriv '(+ (* (** y n) x) (* (** x m) y)) 'x))
