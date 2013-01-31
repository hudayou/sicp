(use-modules (srfi srfi-1))

(define get symbol-property)

(define put set-symbol-property!)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum-package)
  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          (else
            (error "unknown expression type -- DERIV" exp))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (sum? x)
    (pair? x))
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (put '+ 'deriv deriv)
  'done)

(define (install-product-package)
  (define (deriv1 exp var)
    (deriv (cons '* exp) var))
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
                           (multiplicand exp))))))
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (make-acc proc init op args)
    (define (acc args)
      (let* ((folded-args
               (fold
                 (lambda (a previous)
                   (let ((carp (car previous))
                         (cdrp (cdr previous)))
                     (if (number? a)
                       (cons (proc carp a) cdrp)
                       (cons carp (cons a cdrp)))))
                 (list init)
                 (reverse args)))
             (carf (car folded-args))
             (cdrf (cdr folded-args)))
        (cond ((null? cdrf) carf)
              ((= carf init) cdrf)
              (else (cons carf cdrf)))))
    (cond ((null? args) init)
          ((null? (cdr args)) (car args))
          (else
            (let ((acc-of-args (acc args)))
              (cond ((number? acc-of-args) acc-of-args)
                    ((= (length acc-of-args) 1) (car acc-of-args))
                    (else (cons op acc-of-args)))))))
  (define (make-sum . args)
    (make-acc + 0 '+ args))
  (define (addend s) (cadr s))
  (define (augend s) (apply make-sum (cddr s)))
  (define (make-product . args)
    (define (acc args)
      (let* ((folded-args
               (fold
                 (lambda (a previous)
                   (let ((carp (car previous))
                         (cdrp (cdr previous)))
                     (if (number? a)
                       (cons (* carp a) cdrp)
                       (cons carp (cons a cdrp)))))
                 (list 1)
                 (reverse args)))
             (carf (car folded-args))
             (cdrf (cdr folded-args)))
        (cond ((null? cdrf) carf)
              ((= carf 0) carf)
              ((= carf 1) cdrf)
              (else (cons carf cdrf)))))
    (cond ((null? args) 1)
          ((null? (cdr args)) (car args))
          (else
            (let ((acc-of-args (acc args)))
              (cond ((number? acc-of-args) acc-of-args)
                    ((= (length acc-of-args) 1) (car acc-of-args))
                    (else (cons '* acc-of-args)))))))
  (define (multiplier s) (cadr s))
  (define (multiplicand s) (apply make-product (cddr s)))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (put '* 'deriv deriv1)
  'done)

(define (install-exponent-package)
  (define (deriv1 exp var)
    (deriv (cons '** exp) var))
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
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
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
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
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
  (put '** 'deriv deriv1)
  'done)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))