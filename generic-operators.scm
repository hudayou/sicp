(define (square x) (mul x x))

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
(define (neg x) (apply-generic 'neg x))
(define (value x) (apply-generic 'value x))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (same-sign? x y) (apply-generic 'same-sign? x y))
(define (greatest-common-divisor x y)
  (apply-generic 'greatest-common-divisor x y))
(define (reduce p1 p2)
  (apply-generic 'reduce p1 p2))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (sqrtz z) (apply-generic 'sqrtz z))
(define (atanz z1 z2) (apply-generic 'atanz z1 z2))
(define (sinz z) (apply-generic 'sinz z))
(define (cosz z) (apply-generic 'cosz z))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'neg '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'value '(scheme-number)
       (lambda (x) x))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'same-sign? '(scheme-number scheme-number)
       (lambda (x y) (positive? (* x y))))
  (put 'reduce '(scheme-number scheme-number) reduce-integers)
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (x y) (tag (gcd x y))))
  (put 'sqrtz '(scheme-number)
       (lambda (x) (tag (sqrt x))))
  (put 'atanz '(scheme-number scheme-number)
       (lambda (x) (tag (atan x))))
  (put 'cosz '(scheme-number)
       (lambda (x) (tag (cos x))))
  (put 'sinz '(scheme-number)
       (lambda (x) (tag (sin x))))
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
  (define (denom x) (cadr x))
  (define (value x) (div (numer x) (denom x)))
  (define (make-rat n d)
    ;; (let ((g (gcd n d)))
    ;;   (cons (/ n g) (/ d g))))
    (reduce n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'neg '(rational)
       (lambda (x) (tag (make-rat (neg (numer x)) (denom x)))))
  (put 'value '(rational) value)
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equal? x y)))
  (put 'sqrtz '(rational)
       (lambda (x) (tag
                     (make-rat
                       (numerator (sqrt (value x)))
                       (denominator (sqrt (value x)))))))
  (put 'atanz '(rational rational)
       (lambda (x) (tag
                     (make-rat
                       (numerator (atan (value x)))
                       (denominator (atan (value x)))))))
  (put 'cosz '(rational)
       (lambda (x) (tag
                     (make-rat
                       (numerator (cos (value x)))
                       (denominator (cos (value x)))))))
  (put 'sinz '(rational)
       (lambda (x) (tag
                     (make-rat
                       (numerator (sin (value x)))
                       (denominator (sin (value x)))))))
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
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  ;; to be included in the complex package
  (define (add-complex-to-schemenum z x)
    (make-from-real-imag (add (real-part z) x)
                         (imag-part z)))
  (put 'add '(complex scheme-number)
       (lambda (z x) (tag (add-complex-to-schemenum z x))))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'neg '(complex)
       (lambda (z) (tag
                     (make-from-real-imag (neg (real-part z))
                                          (neg (imag-part z))))))
  (put 'value '(complex)
       (lambda (z) (make-rectangular (real-part z) (imag-part z))))
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
  (put 'sqrtz '(complex) sqrtz)
  (put 'atanz '(complex complex) atanz)
  (put 'cosz '(complex) cosz)
  (put 'sinz '(complex) sinz)
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
    (sqrtz (add (square (real-part z))
                (square (imag-part z)))))
  (define (angle z)
    (atanz (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosz a)) (mul r (sinz a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (define (tag-polar x) (attach-tag 'polar x))
  (define (tag-complex x) (attach-tag 'complex x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'sqrtz '(rectangular) sqrtz)
  (put 'atanz '(rectangular rectangular) atanz)
  (put 'cosz '(rectangular) cosz)
  (put 'sinz '(rectangular) sinz)
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
    (mul (magnitude z) (cosz (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sinz (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrtz (add (square x) (square y)))
          (atanz y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'sqrtz '(polar) sqrtz)
  (put 'atanz '(polar polar) atanz)
  (put 'cosz '(polar) cosz)
  (put 'sinz '(polar) sinz)
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

;; Show how to generalize apply-generic to handle coercion in the general case
;; of multiple arguments. One strategy is to attempt to coerce all the
;; arguments to the type of the first argument, then to the type of the second
;; argument, and so on. Give an example of a situation where this strategy (and
;; likewise the two-argument version given above) is not sufficiently general.
;; (Hint: Consider the case where there are some suitable mixed-type operations
;; present in the table that will not be tried.)
;;
;; If there is a type tower, without lose generality, let's assume it's
;; a number tower.
;; We have appropriate operations for two complex numbers, if we have two
;; non complex arguments, then the appropriate operation for complex numbers
;; will not be tried by using the strategy above.

(define (same-type? type-list)
  (cond ((null? type-list) #t)
        ((null? (cdr type-list)) #t)
        (else
          (and
            (eq? (car type-list)
                 (cadr type-list))
            (same-type? (cdr type-list))))))
(define (coercions-available? coercion-list)
  (if (null? coercion-list)
    #t
    (and (car coercion-list)
         (coercions-available? (cdr coercion-list)))))
(define (get-coercion-list from-args to-arg)
  (map
    (lambda (t)
      ;; get coercion from-type->to-type
      (get-coercion t (type-tag to-arg)))
    (map type-tag from-args)))
(define (coercion-args coercion-list args)
  (map
    (lambda (c a)
      (c a))
    coercion-list
    args))
;; (define (find-proc-2 op left-args arg right-args)
;;   (let ((left-coercion-list (get-coercion-list left-args arg))
;;         (right-coercion-list (get-coercion-list right-args arg)))
;;     (if (and
;;           (coercions-available? left-coercion-list)
;;           (coercions-available? right-coercion-list))
;;       (find-proc-0 op
;;                    (append (coercion-args
;;                              left-coercion-list
;;                              left-args)
;;                            (cons arg
;;                                  (coercion-args
;;                                    right-coercion-list
;;                                    right-args))))
;;       #f)))
;; (define (find-proc-1 op left-args rest-args)
;;   (if (null? rest-args)
;;     #f
;;     (let ((arg (car rest-args))
;;           (right-args (cdr rest-args)))
;;       (or
;;         (find-proc-2 op left-args arg right-args)
;;         (find-proc-1 op
;;                      (append left-args (list arg))
;;                      right-args)))))
(define (find-proc-0 op args)
  ;; lookup proc in the operation-and-type table
  ;; retun proc and args if it is found
  ;; otherwise return #f
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (cons proc args)
        #f))))
;; (define (find-proc op args)
;;   ;; look up proc without coercion the args
;;   (let ((proc (find-proc-0 op args)))
;;     (if proc
;;       proc
;;       (let ((type-tags (map type-tag args)))
;;         (if (same-type? type-tags)
;;           #f
;;           ;; look up proc after coercion the args
;;           (find-proc-1 op '() args))))))
;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (find-proc op args)))
;;       (if proc
;;         (apply (car proc) (map contents (cdr proc)))
;;         (error "no method for these types"
;;                (list op type-tags))))))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer (list x)))
  (define (value x)
    (car x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ (value x) (value y)))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- (value x) (value y)))))
  (put 'neg '(integer)
       (lambda (x) (tag (- (value x)))))
  (put 'value '(integer) value)
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* (value x) (value y)))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ (value x) (value y)))))
  (put 'greatest-common-divisor '(integer integer)
       (lambda (x y) (tag (gcd x y))))
  (put 'sqrtz '(integer)
       (lambda (x) (tag (sqrt (value x)))))
  (put 'atanz '(integer integer)
       (lambda (x y) (tag (atan (value x) (value y)))))
  (put 'cosz '(integer)
       (lambda (x) (tag (cos (value x)))))
  (put 'sinz '(integer)
       (lambda (x) (tag (sin (value x)))))
  ;; following added to integer package
  (put 'exp '(integer integer)
       ; using primitive expt
       (lambda (x y) (tag (expt (value x) (value y)))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= (value x) (value y))))
  (put '=zero? '(integer)
       (lambda (x) (zero? (value x))))
  (put 'make 'integer
       (lambda (x) (tag x)))
  'done)

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real (list x)))
  (define (value x)
    (car x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ (value x) (value y)))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- (value x) (value y)))))
  (put 'neg '(real)
       (lambda (x) (tag (- (value x)))))
  (put 'value '(real) value)
  (put 'mul '(real real)
       (lambda (x y) (tag (* (value x) (value y)))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ (value x) (value y)))))
  (put 'sqrtz '(real)
       (lambda (x) (tag (sqrt (value x)))))
  (put 'atanz '(real real)
       (lambda (x y) (tag (atan (value x) (value y)))))
  (put 'cosz '(real)
       (lambda (x) (tag (cos (value x)))))
  (put 'sinz '(real)
       (lambda (x) (tag (sin (value x)))))
  ;; following added to real package
  (put 'exp '(real real)
       ; using primitive expt
       (lambda (x y) (tag (expt (value x) (value y)))))
  (put 'equ? '(real real)
       (lambda (x y) (= (value x) (value y))))
  (put '=zero? '(real)
       (lambda (x) (zero? (value x))))
  (put 'make 'real
       (lambda (x) (tag x)))
  'done)

(install-integer-package)
(install-real-package)

(define (make-real r)
  ((get 'make 'real) r))

(define (make-integer i)
  ((get 'make 'integer) i))

(define (install-tower-package)
  ;; coercion to an intermediate type -- raw number
  (define (raise-scheme-number-to-integer x)
    (make-integer (value (attach-tag 'scheme-number x))))
  (define (raise-integer-to-rational x)
    (make-rational (numerator (value (attach-tag 'integer x)))
                   (denominator (value (attach-tag 'integer x)))))
  (define (raise-rational-to-real x)
    (make-real (value (attach-tag 'rational x))))
  (define (raise-real-to-complex x)
    (make-complex-from-real-imag
      (value (attach-tag 'real x))
      0))
  (define (raise-complex-to-polynomial x)
    (make-polynomial
      'x
      (make-term-list
        (make-term
          0
          (drop
            (attach-tag 'complex x))))))
  (define (project-integer-to-number x)
    (make-scheme-number (value (attach-tag 'integer x))))
  (define (project-rational-to-integer x)
    (make-integer (numerator (value (attach-tag 'rational x)))))
  (define (project-real-to-rational x)
    (make-rational (numerator (value (attach-tag 'real x)))
                   (denominator (value (attach-tag 'real x)))))
  (define (project-complex-to-real x)
    (make-real (real-part (attach-tag 'complex x))))
  (define (project-polynomial-to-complex x)
    (drop (value (attach-tag 'polynomial x))))
  (put 'raise '(scheme-number) raise-scheme-number-to-integer)
  (put 'raise '(integer) raise-integer-to-rational)
  (put 'raise '(rational) raise-rational-to-real)
  (put 'raise '(real) raise-real-to-complex)
  (put 'raise '(complex) raise-complex-to-polynomial)
  (put 'level '(scheme-number) (lambda (x) 0))
  (put 'level '(integer) (lambda (x) 1))
  (put 'level '(rational) (lambda (x) 2))
  (put 'level '(real) (lambda (x) 3))
  (put 'level '(complex) (lambda (x) 4))
  (put 'level '(polynomial) (lambda (x) 5))
  (put 'project '(integer) project-integer-to-number)
  (put 'project '(rational) project-rational-to-integer)
  (put 'project '(real) project-real-to-rational)
  (put 'project '(complex) project-complex-to-real)
  (put 'project '(polynomial) project-polynomial-to-complex)
  'done)

;; data directed generic raise
(define (raise x)
  (apply-generic 'raise x))
(define (level x)
  (apply-generic 'level x))
(define max-level 5)
(define min-level 0)
(define (project x)
  (apply-generic 'project x))
(put 'raise 'not-raisable #t)
(put 'level 'not-raisable #t)

(define (find-highest-level args)
  (let loop ((highest 0)
             (args args))
    (if (null? args)
      highest
      (let ((cara (car args))
            (cdra (cdr args)))
        (if (< highest (level cara))
          (loop (level cara) cdra)
          (loop highest cdra))))))
(define (successive-raise arg lvl)
  (let ((current-level (level arg)))
    (if (= lvl current-level)
      arg
      (successive-raise (raise arg) lvl))))
(define (raise-args args)
  (let ((l (find-highest-level args)))
    (map
      (lambda (a)
        (successive-raise a l))
      args)))
(define (find-proc op args)
  ;; look up proc without coercion the args
  (let ((proc (find-proc-0 op args)))
    (if proc
      proc
      (let ((type-tags (map type-tag args)))
        (if (or (and (same-type? type-tags)
                     (> (length args) 1))
                (get op 'not-raisable))
          #f
          ;; look up proc after coercion the args
          (find-proc-1 op (raise-args args)))))))
(define (find-proc-1 op raised-args)
  (let ((cara (car raised-args))
        (cdra (cdr raised-args)))
    (let ((cara-level (level cara)))
      (if (= cara-level max-level)
        (find-proc-0 op raised-args)
        (let ((proc (find-proc-0 op raised-args)))
          (if proc
            proc
            (find-proc-1 op (raise-args (cons (raise cara) cdra)))))))))

(define (drop x)
  (let loop ((y x)
             (lvl (level x)))
    (if (zero? lvl)
      y
      (let ((project-y (project y)))
        (if (equ? (raise project-y) y)
          (loop project-y (level project-y))
          y)))))

(put 'add 'drop #f)
(put 'sub 'drop #f)
(put 'mul 'drop #f)
(put 'div 'drop #f)

;; special care needs to be taken in apply-generic to avoid
;; generic operations interlock themselves
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (find-proc op args)))
      (if proc
        (let ((result (apply (car proc) (map contents (cdr proc)))))
          (if (get op 'drop)
            (drop result)
            result))
        (error "no method for these types"
               (list op type-tags))))))

(define (polynomial? p)
  (eq? (type-tag p) 'polynomial))

;; (define (install-polynomial-package)
;;   ;; internal procedures
;;   ;; representation of poly
;;   (define (make-poly variable term-list)
;;     (cons variable term-list))
;;   (define (variable p) (car p))
;;   (define (term-list p) (cdr p))
;;   (define (variable? x) (symbol? x))
;;   (define (same-variable? v1 v2)
;;     (and (variable? v1) (variable? v2) (eq? v1 v2)))
;;   (define (make-expanded-term-list . expanded-term-list)
;;     expanded-term-list)
;;   (define (make-expanded-term coeff . var-list)
;;     (cons coeff var-list))
;;   (define (make-var var order)
;;     (list var order))
;;   ;; representation of term
;;   (define (make-term order coeff) (list order coeff))
;;   (define (order term) (car term))
;;   (define (coeff term) (cadr term))
;;   (define (the-empty-termlist) '())
;;   ;; continued on next page
;;   (define (remake-poly p dominant)
;;     (if (same-variable? (variable p) dominant)
;;       p
;;       (rearrange-poly (expand-poly p) dominant)))
;;   (define (expand-poly p)
;;     (expand-terms (variable p) (term-list p)))
;;   (define (expand-terms variable L)
;;     (if (empty-termlist? L)
;;       L
;;       (let ((first (first-term L))
;;             (rest (rest-terms L)))
;;         (append (expand-term variable first)
;;                 (expand-terms variable rest)))))
;;   (define (expand-term variable t)
;;     (let ((order-t (order t))
;;           (coeff-t (coeff t)))
;;       (if (polynomial? coeff-t)
;;         (adjoin-var (make-var variable order-t) (expand coeff-t))
;;         (make-expanded-term-list
;;           (make-expanded-term coeff-t (make-var variable order-t))))))
;;   (define (adjoin-var var expanded-term-list)
;;     (if (null? expanded-term-list)
;;       expanded-term-list
;;       (map (lambda (t) (append t (list var))) expanded-term-list)))
;;   (define (add-poly p1 p2)
;;     (cond ((=zero? (tag p1)) p2)
;;           ((=zero? (tag p2)) p1)
;;           (else
;;             (if (same-variable? (variable p1) (variable p2))
;;               (make-poly (variable p1)
;;                          (add-terms (term-list p1)
;;                                     (term-list p2)))
;;               (error "polys not in same var -- add-poly"
;;                      (list p1 p2))))))
;;   (define (add-terms L1 L2)
;;     (cond ((empty-termlist? L1) L2)
;;           ((empty-termlist? L2) L1)
;;           (else
;;             (let ((t1 (first-term L1)) (t2 (first-term L2)))
;;               (cond ((> (order t1) (order t2))
;;                      (adjoin-term
;;                        t1 (add-terms (rest-terms L1) L2)))
;;                     ((< (order t1) (order t2))
;;                      (adjoin-term
;;                        t2 (add-terms L1 (rest-terms L2))))
;;                     (else
;;                       (adjoin-term
;;                         (make-term (order t1)
;;                                    (add (coeff t1) (coeff t2)))
;;                         (add-terms (rest-terms L1)
;;                                    (rest-terms L2)))))))))
;;   (define (mul-poly p1 p2)
;;     (if (same-variable? (variable p1) (variable p2))
;;       (make-poly (variable p1)
;;                  (mul-terms (term-list p1)
;;                             (term-list p2)))
;;       (error "polys not in same var -- mul-poly"
;;              (list p1 p2))))
;;   (define (mul-terms L1 L2)
;;     (if (empty-termlist? L1)
;;       L1
;;       (add-terms (mul-term-by-all-terms (first-term L1) L2)
;;                  (mul-terms (rest-terms L1) L2))))
;;   (define (mul-term-by-all-terms t1 L)
;;     (if (empty-termlist? L)
;;       L
;;       (let ((t2 (first-term L)))
;;         (adjoin-term
;;           (make-term (+ (order t1) (order t2))
;;                      (mul (coeff t1) (coeff t2)))
;;           (mul-term-by-all-terms t1 (rest-terms L))))))
;;   (define (div-poly p1 p2)
;;     (if (same-variable? (variable p1) (variable p2))
;;       (let ((result
;;               (div-terms (term-list p1)
;;                          (term-list p2))))
;;         (list
;;           (make-poly (variable p1)
;;                      (car result))
;;           (make-poly (variable p1)
;;                      (cadr result))))
;;       (error "polys not in same var -- mul-poly"
;;              (list p1 p2))))
;;   (define (div-terms L1 L2)
;;     (if (empty-termlist? L1)
;;       (list L1 L1)
;;       (let ((t1 (first-term L1))
;;             (t2 (first-term L2)))
;;         (if (> (order t2) (order t1))
;;           (list (attach-tag (type-tag L1) (the-empty-termlist)) L1)
;;           (let ((new-c (div (coeff t1) (coeff t2)))
;;                 (new-o (- (order t1) (order t2))))
;;             (let ((rest-of-result
;;                     (div-terms
;;                       (sub-terms
;;                         L1
;;                         (mul-term-by-all-terms (make-term new-o new-c)
;;                                                L2))
;;                       L2)
;;                     ))
;;               (list (adjoin-term (make-term new-o new-c)
;;                                  (car rest-of-result))
;;                     (cadr rest-of-result))
;;               ))))))
;;   (define (sub-poly p1 p2)
;;     (if (same-variable? (variable p1) (variable p2))
;;       (make-poly (variable p1)
;;                  (sub-terms (term-list p1)
;;                             (term-list p2)))
;;       (error "polys not in same var -- mul-poly"
;;              (list p1 p2))))
;;   (define (sub-terms L1 L2)
;;     (add-terms L1 (neg-terms L2)))
;;   (define (neg-poly p)
;;     (make-poly (variable p)
;;                (neg-terms (term-list p))))
;;   (define (neg-terms L)
;;     (if (empty-termlist? L)
;;       L
;;       (let ((first (first-term L))
;;             (rest (rest-terms L)))
;;         (adjoin-term
;;           (make-term (order first)
;;                      (neg (coeff first)))
;;           (neg-terms rest)))))
;;   (define (find-constant-term p)
;;     (let loop ((term-list (term-list p)))
;;       (if (empty-termlist? term-list)
;;         #f
;;         (let ((first (first-term term-list))
;;               (rest (rest-terms term-list)))
;;           (if (zero? (order first))
;;             first
;;             (loop rest))))))
;;   (define (value p)
;;     (let ((real 0)
;;           (imag 0))
;;       (if (not (=zero? (attach-tag 'polynomial p)))
;;         (let ((const-term (find-constant-term p)))
;;           (if const-term
;;             (let ((coeff (coeff const-term)))
;;               (if (not (eq? (type-tag coeff) 'polynomial))
;;                 (begin
;;                   (set! real (real-part coeff))
;;                   (set! imag (imag-part coeff))))))))
;;       (make-complex-from-real-imag real imag)))
;;   ;; interface to rest of the system
;;   (define (tag p) (attach-tag 'polynomial p))
;;   (put 'add '(polynomial polynomial)
;;        (lambda (p1 p2) (tag (add-poly p1 p2))))
;;   (put 'sub '(polynomial polynomial)
;;        (lambda (p1 p2) (tag (sub-poly p1 p2))))
;;   (put 'neg '(polynomial)
;;        (lambda (p) (tag (neg-poly p))))
;;   (put 'value '(polynomial) value)
;;   (put 'mul '(polynomial polynomial)
;;        (lambda (p1 p2) (tag (mul-poly p1 p2))))
;;   (put 'div '(polynomial polynomial)
;;        (lambda (p1 p2) (list (tag (car (div-poly p1 p2)))
;;                              (tag (cadr (div-poly p1 p2))))))
;;   (put '=zero? '(polynomial)
;;        (lambda (p)
;;          (let loop ((term-list (term-list p)))
;;            (if (empty-termlist? term-list)
;;              #t
;;              (let ((first (first-term term-list))
;;                    (rest (rest-terms term-list)))
;;                (and (=zero? (coeff first))
;;                     (loop rest)))))))
;;   (put 'equ? '(polynomial polynomial)
;;        (lambda (p1 p2)
;;          (let ((variable-of-p1 (variable p1))
;;                (variable-of-p2 (variable p2)))
;;            (and (eq? variable-of-p1 variable-of-p2)
;;                 (let loop ((term-list-of-p1 (term-list p1))
;;                            (term-list-of-p2 (term-list p2)))
;;                   (if (and (empty-termlist? term-list-of-p1)
;;                            (empty-termlist? term-list-of-p2))
;;                     #t
;;                     (and (= (length term-list-of-p1)
;;                             (length term-list-of-p2))
;;                          (let ((first-of-p1 (first-term term-list-of-p1))
;;                                (rest-of-p1 (rest-terms term-list-of-p1))
;;                                (first-of-p2 (first-term term-list-of-p2))
;;                                (rest-of-p2 (rest-terms term-list-of-p2)))
;;                            (and (= (order first-of-p1)
;;                                    (order first-of-p2))
;;                                 (equ? (coeff first-of-p1)
;;                                       (coeff first-of-p2))
;;                                 (loop rest-of-p1 rest-of-p2))))))))))
;;   (put 'make 'polynomial
;;        (lambda (var terms) (tag (make-poly var terms))))
;;   (put 'expand '(polynomial) expand-poly)
;;   (put 'make-term 'polynomial make-term)
;;   (put 'order 'polynomial order)
;;   (put 'coeff 'polynomial coeff)
;;   (put 'the-empty-termlist 'polynomial the-empty-termlist)
;;   (put 'make-dense-term-list 'polynomial
;;        (lambda (terms)
;;          ((get 'make-term-list 'dense) terms)))
;;   (put 'make-sparse-term-list 'polynomial
;;        (lambda (terms)
;;          ((get 'make-term-list 'sparse) terms)))
;;   'done)
;;
;; (define (install-sparse-package)
;;   ;; sparse representation of term lists
;;   (define (tag x)
;;     (attach-tag 'sparse x))
;;   (define (adjoin-term term term-list)
;;     (if (=zero? (coeff term))
;;       term-list
;;       (cons term term-list)))
;;   (define (first-term term-list) (car term-list))
;;   (define (rest-terms term-list) (cdr term-list))
;;   (define (empty-termlist? term-list) (null? term-list))
;;   (define (make-term-list terms) terms)
;;   (put 'adjoin-term '(sparse sparse)
;;        (lambda (term term-list)
;;          (tag (adjoin-term term term-list))))
;;   (put 'first-term '(sparse)
;;        (lambda (term-list)
;;          (first-term term-list)))
;;   (put 'rest-terms '(sparse)
;;        (lambda (term-list)
;;          (tag (rest-terms term-list))))
;;   (put 'empty-termlist? '(sparse) empty-termlist?)
;;   (put 'make-term-list 'sparse
;;        (lambda (terms)
;;          (tag (make-term-list terms))))
;;   'done)
;;
;; (define (install-dense-package)
;;   ;; dense representation of term list
;;   (define (tag x)
;;     (attach-tag 'dense x))
;;   (define (adjoin-term term term-list)
;;     (if (=zero? (coeff term))
;;       term-list
;;       (let loop ((i (length term-list))
;;                  (term-list term-list))
;;         (cond ((= i (order term)) (cons (coeff term) term-list))
;;               ((> i (order term)) (error "bad usage of adjoin-term"
;;                                          (list term term-list)))
;;               (else
;;                 (loop (+ i 1) (cons 0 term-list)))))))
;;   (define (first-term term-list)
;;     (make-term
;;       (length (cdr term-list))
;;       (car term-list)))
;;   (define (rest-terms term-list) (cdr term-list))
;;   (define (empty-termlist? term-list) (null? term-list))
;;   (define (make-term-list terms)
;;     (if (null? terms)
;;       (the-empty-termlist)
;;       (adjoin-term (car terms)
;;                    (make-term-list (cdr terms)))))
;;   (put 'adjoin-term '(dense dense)
;;        (lambda (term term-list)
;;          (tag (adjoin-term term term-list))))
;;   (put 'first-term '(dense)
;;        (lambda (term-list)
;;          (first-term term-list)))
;;   (put 'rest-terms '(dense)
;;        (lambda (term-list)
;;          (tag (rest-terms term-list))))
;;   (put 'empty-termlist? '(dense) empty-termlist?)
;;   (put 'make-term-list 'dense
;;        (lambda (terms)
;;          (tag make-term-list)))
;;   'done)
;;
;; (define (make-polynomial var terms)
;;   ((get 'make 'polynomial) var terms))
;;
;; (define (make-term order coeff)
;;   ((get 'make-term 'polynomial) order coeff))
;;
;; (define (order term)
;;   ((get 'order 'polynomial) term))
;;
;; (define (coeff term)
;;   ((get 'coeff 'polynomial) term))
;;
;; (define (the-empty-termlist)
;;   ((get 'the-empty-termlist 'polynomial)))
;;
;; (define (make-dense-term-list . terms)
;;   ((get 'make-dense-term-list 'polynomial) terms))
;;
;; (define (make-sparse-term-list . terms)
;;   ((get 'make-sparse-term-list 'polynomial) terms))
;;
;; (define (adjoin-term term term-list)
;;   (apply-generic 'adjoin-term
;;                  (attach-tag (type-tag term-list)
;;                              term)
;;                  term-list))
;; (define (first-term term-list)
;;   (apply-generic 'first-term term-list))
;; (define (rest-terms term-list)
;;   (apply-generic 'rest-terms term-list))
;; (define (empty-termlist? term-list)
;;   (apply-generic 'empty-termlist? term-list))
;;
;; (define (expand p)
;;   (apply-generic 'expand p))
;;
;; (install-dense-package)
;; (install-sparse-package)
;; (install-polynomial-package)
;; (install-tower-package)
;;
;; (use-modules (ice-9 pretty-print))
;;
;; (define poly1
;;   (make-polynomial
;;     'x
;;     (make-sparse-term-list
;;       (make-term 2 (make-polynomial
;;                      'y
;;                      (make-sparse-term-list (make-term 1 1)
;;                                             (make-term 0 1))))
;;       (make-term 1 (make-polynomial
;;                      'y
;;                      (make-sparse-term-list (make-term 2 1)
;;                                             (make-term 0 1))))
;;       (make-term 0 (make-polynomial
;;                      'y
;;                      (make-sparse-term-list (make-term 1 1)
;;                                             (make-term 0 -1)))))))
;;
;; (define poly2
;;   (make-polynomial
;;     'x
;;     (make-sparse-term-list
;;       (make-term 1 (make-polynomial
;;                      'y
;;                      (make-sparse-term-list (make-term 1 1)
;;                                             (make-term 0 -2))))
;;       (make-term 0 (make-polynomial
;;                      'y
;;                      (make-sparse-term-list (make-term 3 1)
;;                                             (make-term 0 7)))))))
;;
;; (define poly3
;;   (make-polynomial
;;     'x
;;     (make-sparse-term-list
;;       (make-term 5 1)
;;       (make-term 0 -1))))
;;
;; (define poly4
;;   (make-polynomial
;;     'x
;;     (make-sparse-term-list
;;       (make-term 2 1)
;;       (make-term 0 -1))))
;;
;; (define poly5
;;   (make-polynomial
;;     'y
;;     (make-sparse-term-list
;;       (make-term 5 1)
;;       (make-term 0 -1))))
;;
;; (define poly6
;;   (make-polynomial
;;     'z
;;     (make-sparse-term-list
;;       (make-term 2 1)
;;       (make-term 0 -1))))

;; (1 (y 1) (x 2))
;; (1 (x 2))
;; (1 (y 2) (x 1))
;; (1 (x 1))
;; (1 (y 1))
;; (-1)
;;
;; (1 (y 1) (x 1))
;; (-2 (x 1))
;; (1 (y 3))
;; (7)

;; ((1 (y 1) (x 2))
;;  (1 (y 0) (x 2))
;;  (1 (y 2) (x 1))
;;  (1 (y 0) (x 1))
;;  (1 (y 1) (x 0))
;;  (-1 (y 0) (x 0)))
;;
;; ((1 (y 1) (x 1))
;;  (-2 (y 0) (x 1))
;;  (1 (y 3) (x 0))
;;  (7 (y 0) (x 0)))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (make-expanded-term-list . expanded-term-list)
    expanded-term-list)
  (define (make-expanded-term coeff . var-list)
    (cons coeff var-list))
  (define (make-var var order)
    (list var order))
  (define (v var)
    (car var))
  (define (o var)
    (cadr var))
  (define (order-var var)
    (cond ((eq? var 'x) 5)
          ((eq? var 'y) 4)
          ((eq? var 'z) 3)
          ((eq? var 'u) 2)
          ((eq? var 'v) 1)
          ((eq? var 'w) 0)
          (else
            (random:uniform))))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (make-term-list terms) terms)
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; continued on next page
  (define (simplify-poly p)
    (let ((terms (term-list p)))
      (if (empty-termlist? (rest-terms terms))
        (let ((first-order (order (first-term terms)))
              (first-coeff (coeff (first-term terms))))
          (if (zero? first-order)
            (if (polynomial? first-coeff)
              (simplify first-coeff)
              first-coeff)
            (tag p)))
        (make-polynomial (variable p)
                         (map simplify-term terms)))))
  (define (simplify-term term)
    (let ((order (order term))
          (coeff (coeff term)))
      (if (polynomial? coeff)
        (make-term order (simplify coeff))
        term)))
  (define (remake-poly p dominant common)
    (rearrange-poly (expand-poly p) dominant common))
  (define (rearrange-poly expanded-term-list dominant common)
    (make-poly dominant
               (combine-same-order-terms
                 (sort-terms
                   (map
                     (lambda (et)
                       (expanded-term->term et dominant common))
                     expanded-term-list)))))
  (define (expanded-term->term expanded-term dominant common)
    (let ((d-coeff (car expanded-term))
          (d-order (get-dominant-order expanded-term dominant)))
      (make-term d-order
                 (var-list->coeff
                   d-coeff
                   (sort-var-list
                     (generalize-var-list
                       (remove-dominant-var-and-coeff expanded-term
                                                      dominant)
                       common))))))
  (define (generalize-var-list var-list common)
    (let ((base (map v var-list)))
      (let loop ((var-list var-list)
                 (common common))
        (if (null? common)
          var-list
          (let ((carc (car common))
                (cdrc (cdr common)))
            (if (memq carc base)
              (loop var-list cdrc)
              (loop (cons (make-var carc 0)
                          var-list)
                    cdrc)))))))
  (define (find-common-vars poly1 poly2)
    (let ((expanded-poly-1 (expand-poly poly1))
          (expanded-poly-2 (expand-poly poly2)))
      (let ((list-of-var-list-1 (map cdr expanded-poly-1))
            (list-of-var-list-2 (map cdr expanded-poly-2)))
        (rip-vars list-of-var-list-1
                  (rip-vars list-of-var-list-2 '())))))
  (define (rip-vars list-of-var-list init)
    (let ((vars init))
      (for-each
        (lambda (var-list)
          (for-each
            (lambda (var)
              (if (not (memq (v var) vars))
                (set! vars (cons (v var) vars))))
            var-list))
        list-of-var-list)
      vars))
  (define (remove-dominant-var-and-coeff expanded-term dominant)
    (let ((var-list (cdr expanded-term)))
      (car (get-dominant-and-other var-list dominant))))
  (define (get-dominant-order expanded-term dominant)
    (let ((var-list (cdr expanded-term)))
      (let ((d-var (cadr (get-dominant-and-other var-list dominant))))
        (if (null? d-var)
          0
          (o d-var)))))
  (define (get-dominant-and-other var-list dominant)
    (let loop ((other '())
               (d '())
               (var-list var-list))
      (cond ((null? var-list) (list other d))
            ((eq? (v (car var-list)) dominant) (loop other
                                                     (car var-list)
                                                     (cdr var-list)))
            (else
              (loop (append other (list (car var-list)))
                    d
                    (cdr var-list))))))
  (define (sort-var-list var-list)
    (stable-sort var-list
                 (lambda (a-var b-var)
                   (< (order-var (v a-var))
                      (order-var (v b-var))))))
  (define (sort-terms term-list)
    (stable-sort term-list
                 (lambda (a-term b-term)
                   (< (order a-term)
                      (order b-term)))))
  (define (combine-same-order-terms term-list)
    (define (combine result term-list)
      (cond ((null? term-list) result)
            ((null? result) (combine (cons (car term-list) result)
                                     (cdr term-list)))
            ((= (order (car result))
                (order (car term-list)))
             (combine (cons (make-term (order (car result))
                                       (add (coeff (car result))
                                            (coeff (car term-list))))
                            (cdr result))
                      (cdr term-list)))
            (else
              (combine (cons (car term-list) result)
                       (cdr term-list)))))
    (combine '() term-list))
  (define (var-list->coeff d-coeff var-list)
    (if (null? var-list)
      d-coeff
      (var-list->polynomial d-coeff var-list)))
  (define (var-list->polynomial d-coeff var-list)
    (define (loop vlist p)
      (if (null? vlist)
        p
        (let ((carv (car vlist))
              (cdrv (cdr vlist)))
          (loop cdrv
                (make-polynomial (v carv)
                                 (list
                                   (make-term (o carv) p)))))))
    (let ((carv (car var-list))
          (cdrv (cdr var-list)))
      (let ((p (make-polynomial (v carv)
                                (list
                                  (make-term (o carv) d-coeff)))))
        (loop cdrv p))))
  (define (expand-poly p)
    (expand-terms (variable p) (term-list p)))
  (define (expand-terms variable L)
    (if (empty-termlist? L)
      L
      (let ((first (first-term L))
            (rest (rest-terms L)))
        (append (expand-term variable first)
                (expand-terms variable rest)))))
  (define (expand-term variable t)
    (let ((order-t (order t))
          (coeff-t (coeff t)))
      (if (zero? order-t)
        (if (polynomial? coeff-t)
          (expand coeff-t)
          (make-expanded-term-list
            (make-expanded-term coeff-t)))
        (if (polynomial? coeff-t)
          (adjoin-var (make-var variable order-t) (expand coeff-t))
          (make-expanded-term-list
            (make-expanded-term coeff-t (make-var variable order-t)))))))
  (define (adjoin-var var expanded-term-list)
    (if (null? expanded-term-list)
      expanded-term-list
      (map (lambda (t) (append t (list var))) expanded-term-list)))
  (define (add-poly p1 p2)
    (cond ((=zero? (tag p1)) p2)
          ((=zero? (tag p2)) p1)
          (else
            (let ((v1 (variable p1))
                  (v2 (variable p2)))
              (if (same-variable? v1 v2)
                (make-poly (variable p1)
                           (add-terms (term-list p1)
                                      (term-list p2)))
                (let ((o1 (order-var v1))
                      (o2 (order-var v2))
                      (common (find-common-vars p1 p2)))
                  (if (< o1 o2)
                    (make-poly
                      v2
                      (add-terms (term-list
                                   (remake-poly p1 v2 (delq v2 common)))
                                 (term-list
                                   (remake-poly p2 v2 (delq v2 common)))))
                    (make-poly
                      v1
                      (add-terms (term-list
                                   (remake-poly p1 v1 (delq v1 common)))
                                 (term-list
                                   (remake-poly
                                     p2 v1 (delq v1 common))))))))))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  (define (mul-poly p1 p2)
    (let ((v1 (variable p1))
          (v2 (variable p2)))
      (if (same-variable? v1 v2)
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (let ((o1 (order-var v1))
              (o2 (order-var v2))
              (common (find-common-vars p1 p2)))
          (if (< o1 o2)
            (make-poly
              v2
              (mul-terms (term-list
                           (remake-poly p1 v2 (delq v2 common)))
                         (term-list
                           (remake-poly p2 v2 (delq v2 common)))))
            (make-poly
              v1
              (mul-terms (term-list
                           (remake-poly p1 v1 (delq v1 common)))
                         (term-list
                           (remake-poly
                             p2 v1 (delq v1 common))))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let ((result
              (div-terms (term-list p1)
                         (term-list p2))))
        (list
          (make-poly (variable p1)
                     (car result))
          (make-poly (variable p1)
                     (cadr result))))
      (error "polys not in same var -- mul-poly"
             (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
          (list (the-empty-termlist) L1)
          (let ((new-c (div (coeff t1) (coeff t2)))
                (new-o (- (order t1) (order t2))))
            (let ((rest-of-result
                    (div-terms
                      (sub-terms
                        L1
                        (mul-term-by-all-terms (make-term new-o new-c)
                                               L2))
                      L2)
                    ))
              (list (adjoin-term (make-term new-o new-c)
                                 (car rest-of-result))
                    (cadr rest-of-result))
              ))))))
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  (define (sub-terms L1 L2)
    (add-terms L1 (neg-terms L2)))
  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))
  (define (neg-terms L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((first (first-term L))
            (rest (rest-terms L)))
        (adjoin-term
          (make-term (order first)
                     (neg (coeff first)))
          (neg-terms rest)))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
      (remove-gcd a)
      ;;(gcd-terms b (remainder-terms a b))))
      (gcd-terms b (pseudoremainder-terms a b))))
  (define (remove-gcd L)
    (if (empty-termlist? L)
      L
      (let ((gcd-c (apply gcd (map coeff L))))
        (map
          (lambda (t)
            (make-term (order t)
                       (/ (coeff t) gcd-c)))
          L))))
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  (define (pseudoremainder-terms a b)
    (let ((first-a (first-term a))
          (first-b (first-term b)))
      (let ((order-a (order first-a))
            (order-b (order first-b))
            (coeff-b (coeff first-b)))
        (let ((factor (expt coeff-b (+ order-a (- order-b) 1))))
          (cadr (div-terms (mul-term-by-all-terms (make-term 0 factor)
                                                  a) b))))))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1)
                            (term-list p2)))
      (error "polys not in same var -- gcd-poly"
             (list p1 p2))))
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (let ((result
              (reduce-terms (term-list p1)
                            (term-list p2))))
        (list
          (make-poly (variable p1)
                     (car result))
          (make-poly (variable p1)
                     (cadr result))))
      (error "polys not in same var -- reduce-poly"
             (list p1 p2))))
  (define (reduce-terms n d)
    (let ((g (gcd-terms n d)))
      (let ((first-n (first-term n))
            (first-d (first-term d))
            (first-g (first-term g)))
        (let ((order-n (order first-n))
              (order-d (order first-d))
              (order-g (order first-g))
              (coeff-g (coeff first-g)))
          (let ((factor (expt coeff-g (+ (max order-n
                                              order-d)
                                         (- order-g)
                                         1))))
            (list
              (remove-gcd
                (car (div-terms (mul-term-by-all-terms (make-term 0 factor)
                                                       n) g)))
              (remove-gcd
                (car (div-terms (mul-term-by-all-terms (make-term 0 factor)
                                                       d) g)))))))))
  (define (find-constant-term p)
    (let loop ((term-list (term-list p)))
      (if (empty-termlist? term-list)
        #f
        (let ((first (first-term term-list))
              (rest (rest-terms term-list)))
          (if (zero? (order first))
            first
            (loop rest))))))
  (define (value p)
    (let ((real 0)
          (imag 0))
      (if (not (=zero? (attach-tag 'polynomial p)))
        (let ((const-term (find-constant-term p)))
          (if const-term
            (let ((coeff (coeff const-term)))
              (if (not (eq? (type-tag coeff) 'polynomial))
                (begin
                  (set! real (real-part coeff))
                  (set! imag (imag-part coeff))))))))
      (make-complex-from-real-imag real imag)))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'neg '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put 'value '(polynomial) value)
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (list (tag (car (div-poly p1 p2)))
                             (tag (cadr (div-poly p1 p2))))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) (list (tag (car (reduce-poly p1 p2)))
                             (tag (cadr (reduce-poly p1 p2))))))
  (put '=zero? '(polynomial)
       (lambda (p)
         (let loop ((term-list (term-list p)))
           (if (empty-termlist? term-list)
             #t
             (let ((first (first-term term-list))
                   (rest (rest-terms term-list)))
               (and (=zero? (coeff first))
                    (loop rest)))))))
  (put 'same-sign? '(polynomial polynomial)
       (lambda (p1 p2)
         (same-sign? (coeff (first-term (term-list p1)))
                     (coeff (first-term (term-list p2))))))
  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((variable-of-p1 (variable p1))
               (variable-of-p2 (variable p2)))
           (and (eq? variable-of-p1 variable-of-p2)
                (let loop ((term-list-of-p1 (term-list p1))
                           (term-list-of-p2 (term-list p2)))
                  (if (and (empty-termlist? term-list-of-p1)
                           (empty-termlist? term-list-of-p2))
                    #t
                    (and (= (length term-list-of-p1)
                            (length term-list-of-p2))
                         (let ((first-of-p1 (first-term term-list-of-p1))
                               (rest-of-p1 (rest-terms term-list-of-p1))
                               (first-of-p2 (first-term term-list-of-p2))
                               (rest-of-p2 (rest-terms term-list-of-p2)))
                           (and (= (order first-of-p1)
                                   (order first-of-p2))
                                (equ? (coeff first-of-p1)
                                      (coeff first-of-p2))
                                (loop rest-of-p1 rest-of-p2))))))))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'expand '(polynomial) expand-poly)
  (put 'simplify '(polynomial) simplify-poly)
  (put 'rearrange 'polynomial rearrange-poly)
  (put 'find-common-vars '(polynomial polynomial) find-common-vars)
  (put 'make 'term
       (lambda (order coeff) (make-term order coeff)))
  (put 'make 'term-list
       (lambda (terms) (make-term-list terms)))
  'done)

(install-polynomial-package)
(install-tower-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (make-term order coeff)
  ((get 'make 'term) order coeff))

(define (make-term-list . terms)
  ((get 'make 'term-list) terms))

(define (expand p)
  (apply-generic 'expand p))

(define (simplify p)
  (apply-generic 'simplify p))

(define (find-common-vars p1 p2)
  (apply-generic 'find-common-vars p1 p2))

(define (rearrange ep d c)
  ((get 'rearrange 'polynomial) ep d c))

(use-modules (ice-9 pretty-print))

(define poly1
  (make-polynomial
    'x
    (make-term-list (make-term 2 (make-polynomial
                                   'y
                                   (make-term-list (make-term 1 1)
                                                   (make-term 0 1))))
                    (make-term 1 (make-polynomial
                                   'y
                                   (make-term-list (make-term 2 1)
                                                   (make-term 0 1))))
                    (make-term 0 (make-polynomial
                                   'y
                                   (make-term-list (make-term 1 1)
                                                   (make-term 0 -1)))))))

(define poly2
  (make-polynomial
    'x
    (make-term-list (make-term 1 (make-polynomial
                                   'y
                                   (make-term-list (make-term 1 1)
                                                   (make-term 0 -2))))
                    (make-term 0 (make-polynomial
                                   'y
                                   (make-term-list (make-term 3 1)
                                                   (make-term 0 7)))))))

(define poly3
  (make-polynomial
    'x
    (make-term-list
      (make-term 5 1)
      (make-term 0 -1))))

(define poly4
  (make-polynomial
    'x
    (make-term-list
      (make-term 2 1)
      (make-term 0 -1))))

(define poly5
  (make-polynomial
    'y
    (make-term-list
      (make-term 5 1)
      (make-term 0 -1))))

(define poly6
  (make-polynomial
    'z
    (make-term-list
      (make-term 2 1)
      (make-term 0 -1))))

(define poly7
  (make-polynomial
    'u
    (make-term-list (make-term 3 poly1)
                    (make-term 2 poly3)
                    (make-term 1 poly5)
                    (make-term 0 poly6))))

(define poly8
  (make-polynomial
    'v
    (make-term-list (make-term 1 poly2)
                    (make-term 0 poly4))))

(pretty-print
  (mul poly1 poly2))

(pretty-print
  (div poly3 poly4))

(define p1 (make-polynomial 'x
                            (make-term-list
                              (make-term 2 1)
                              (make-term 0 1))))
(define p2 (make-polynomial 'x
                            (make-term-list
                              (make-term 3 1)
                              (make-term 0 1))))
(define rf (make-rational p2 p1))

(pretty-print rf)

(pretty-print (add rf rf))

(define p1 (make-polynomial 'x
                            (make-term-list
                              (make-term 4 1)
                              (make-term 3 -1)
                              (make-term 2 -2)
                              (make-term 1 2))))
(define p2 (make-polynomial 'x
                            (make-term-list
                              (make-term 3 1)
                              (make-term 1 -1))))
(pretty-print (greatest-common-divisor p1 p2))

(define p1 (make-polynomial 'x
                            (make-term-list
                              (make-term 2 1)
                              (make-term 1 -2)
                              (make-term 0 1))))
(define p2 (make-polynomial 'x
                            (make-term-list
                              (make-term 2 11)
                              (make-term 0 7))))
(define p3 (make-polynomial 'x
                            (make-term-list
                              (make-term 1 13)
                              (make-term 0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))
(pretty-print (greatest-common-divisor q1 q2))

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(pretty-print rf1)
(pretty-print rf2)
(pretty-print (add rf1 rf2))
