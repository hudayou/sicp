(define (square x) (* x x))

(define (get symbol property)
  (symbol-property symbol property))

(define (put symbol property value)
  (set-symbol-property! symbol property value))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "no method for these types -- apply-generic"
          (list op type-tags))))))

(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
          (error "bad tagged datum -- type-tag" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
          (error "bad tagged datum -- contents" datum))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  ;; following added to Scheme-number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (zero? x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equal? x y)))
  (put '=zero? '(rational)
       (lambda (x) (zero? (numer x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  ;; to be included in the complex package
  (define (add-complex-to-schemenum z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))
  (put 'add '(complex scheme-number)
       (lambda (z x) (tag (add-complex-to-schemenum z x))))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ? z1 z2)))
  (put '=zero? '(complex)
       (lambda (z) (=zero? z)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (define (tag-polar x) (attach-tag 'polar x))
  (define (tag-complex x) (attach-tag 'complex x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'equ? '(rectangular rectangular)
       (lambda (z1 z2) (equal? z1 z2)))
  (put '=zero? '(rectangular)
       (lambda (z) (and (zero? (real-part z))
                        (zero? (imag-part z)))))
  (put 'equ? '(rectangular polar)
       (lambda (z1 z2) (equ? (make-complex-from-mag-ang (magnitude z1)
                                                        (angle z1))
                             (tag-complex (tag-polar z2)))))
  (put 'equ? '(polar rectangular)
       (lambda (z1 z2) (equ?  (tag-complex (tag-polar z1))
                              (make-complex-from-mag-ang (magnitude z2)
                                                         (angle z2)))))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'equ? '(polar polar)
       (lambda (z1 z2) (equal? z1 z2)))
  (put '=zero? '(polar)
       (lambda (z) (zero? (magnitude z))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)

;; guile> (trace apply-generic)
;; (apply-generic)
;; guile> (magnitude (make-complex-from-real-imag 3 4))
;; [apply-generic magnitude (complex rectangular 3 . 4)]
;; [apply-generic magnitude (rectangular 3 . 4)]
;; 5.0
;; 5.0
;; guile> (symbol-pref 'magnitude)
;; (((complex) . #<procedure magnitude (z)>) ((rectangular) . #<procedure
;; magnitude (z)>) ((polar) . #<procedure magnitude (z)>))

(=zero? 0)
(=zero? 1)
(=zero? (make-rational 0 1))
(=zero? (make-rational 1 1))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-real-imag 0 1))
(=zero? (make-complex-from-real-imag 1 0))
(=zero? (make-complex-from-real-imag 1 1))
(=zero? (make-complex-from-mag-ang 0 0))
(=zero? (make-complex-from-mag-ang 0 1))
(=zero? (make-complex-from-mag-ang 1 0))
(=zero? (make-complex-from-mag-ang 1 1))

;; the design of coercion procedures depends on the structure of the
;; types, or how you think about the relations between the types.

(define get-coercion symbol-property)
(define put-coercion set-symbol-property!)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (not (eq? type1 type2))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a2)))))))))
      (error "no method for these types"
             (list op type-tags)))))

;; traces about coercion
;; guile> (trace apply-generic)
;; (apply-generic)
;; guile> (add (make-complex-from-real-imag 3 4) 4)
;; [apply-generic add (complex rectangular 3 . 4) 4]
;; |  [apply-generic real-part (rectangular 3 . 4)]
;; |  3
;; |  [apply-generic imag-part (rectangular 3 . 4)]
;; |  4
;; (complex rectangular 7 . 4)
;; (complex rectangular 7 . 4)
;; guile> (sub (make-complex-from-real-imag 3 4) 4)
;; [apply-generic sub (complex rectangular 3 . 4) 4]
;; [apply-generic sub (complex rectangular 3 . 4) (complex rectangular 4 . 0)]
;; |  [apply-generic real-part (rectangular 3 . 4)]
;; |  3
;; |  [apply-generic real-part (rectangular 4 . 0)]
;; |  4
;; |  [apply-generic imag-part (rectangular 3 . 4)]
;; |  4
;; |  [apply-generic imag-part (rectangular 4 . 0)]
;; |  0
;; (complex rectangular -1 . 4)
;; (complex rectangular -1 . 4)

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))
