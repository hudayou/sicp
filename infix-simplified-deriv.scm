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
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (define (s? x)
    (if (= (length x) 1)
      #f
      (or (eq? (cadr x) '+)
          (s? (cddr x)))))
  (and (pair? x)
       (s? x)))

(define (sub? x)
  (and (pair? x) (eq? (cadr x) '-)))

;; x is a product when and only when all cddr of x is a product
;; assume x is a legal expression
(define (product? x)
  (define (prod? x)
    (if (= (length x) 1)
      #t
      (and (eq? (cadr x) '*)
           (prod? (cddr x)))))
  (and (pair? x)
       (prod? x)))

(define (split-sum sum)
  (define (split reversed-addend augend)
    (if (eq? (car augend) '+)
      (cons (reverse reversed-addend) (cdr augend))
      (split (cons (car augend) reversed-addend)
             (cdr augend))))
    (split '() sum))

(define (addend s)
  (let ((a1 (car (split-sum s))))
    (if (= (length a1) 1)
      (car a1)
      a1)))

(define (augend s)
  (let ((a2 (cdr (split-sum s))))
    (if (= (length a2) 1)
      (car a2)
      a2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(use-modules (srfi srfi-1))

(define (make-sum . args)
  (define (acc args)
    (let* ((folded-args
             (fold
               (lambda (a previous)
                 (let ((carp (car previous))
                       (cdrp (cdr previous)))
                   (cond ((number? a) (cons (+ a carp) cdrp))
                         ((null? cdrp) (cons carp (cons a cdrp)))
                         (else (cons carp (append (list a '+) cdrp))))))
               (list 0)
               (reverse args)))
           (carf (car folded-args))
           (cdrf (cdr folded-args)))
      (cond ((null? cdrf) carf)
            ((= carf 0) cdrf)
            (else (append (list carf '+) cdrf)))))
  (cond ((null? args) 0)
        ((null? (cdr args)) (car args))
        (else
          (let ((acc-of-args (acc args)))
            (cond ((number? acc-of-args) acc-of-args)
                  ((= (length acc-of-args) 1) (car acc-of-args))
                  (else acc-of-args))))))

(define (multiplier p) (car p))

(define (multiplicand p)
  (let ((cddrp (cddr p)))
    (if (= (length cddrp) 1)
      (car cddrp)
      cddrp)))

(define (make-product . args)
  (define (acc args)
    (let* ((folded-args
             (fold
               (lambda (a previous)
                 (let ((carp (car previous))
                       (cdrp (cdr previous)))
                   (cond ((number? a) (cons (* a carp) cdrp))
                         ((null? cdrp) (cons carp (cons a cdrp)))
                         (else (cons carp (append (list a '*) cdrp))))))
               (list 1)
               (reverse args)))
           (carf (car folded-args))
           (cdrf (cdr folded-args)))
      (cond ((null? cdrf) carf)
            ((= carf 0) carf)
            ((= carf 1) cdrf)
            (else (append (list carf '*) cdrf)))))
  (cond ((null? args) 1)
        ((null? (cdr args)) (car args))
        (else
          (let ((acc-of-args (acc args)))
            (cond ((number? acc-of-args) acc-of-args)
                  ((= (length acc-of-args) 1) (car acc-of-args))
                  (else acc-of-args))))))

(use-modules (ice-9 pretty-print))

(pretty-print (deriv '(x + (3 * (x + (y + 2)))) 'x))
(pretty-print (deriv '((x * y) * (x + 3)) 'x))
