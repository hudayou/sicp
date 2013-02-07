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
  (define (denom x) (cdr x))
  (define (value x) (/ (numer x) (denom x)))
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
  (put 'neg '(rational)
       (lambda (x) (tag (make-rat (- (numer x)) (denom x)))))
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
(define (find-proc-2 op left-args arg right-args)
  (let ((left-coercion-list (get-coercion-list left-args arg))
        (right-coercion-list (get-coercion-list right-args arg)))
    (if (and
          (coercions-available? left-coercion-list)
          (coercions-available? right-coercion-list))
      (find-proc-0 op
                   (append (coercion-args
                             left-coercion-list
                             left-args)
                           (cons arg
                                 (coercion-args
                                   right-coercion-list
                                   right-args))))
      #f)))
(define (find-proc-1 op left-args rest-args)
  (if (null? rest-args)
    #f
    (let ((arg (car rest-args))
          (right-args (cdr rest-args)))
      (or
        (find-proc-2 op left-args arg right-args)
        (find-proc-1 op
                     (append left-args (list arg))
                     right-args)))))
(define (find-proc-0 op args)
  ;; lookup proc in the operation-and-type table
  ;; retun proc and args if it is found
  ;; otherwise return #f
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (cons proc args)
        #f))))
(define (find-proc op args)
  ;; look up proc without coercion the args
  (let ((proc (find-proc-0 op args)))
    (if proc
      proc
      (let ((type-tags (map type-tag args)))
        (if (same-type? type-tags)
          #f
          ;; look up proc after coercion the args
          (find-proc-1 op '() args))))))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (find-proc op args)))
      (if proc
        (apply (car proc) (map contents (cdr proc)))
        (error "no method for these types"
               (list op type-tags))))))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
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
       (lambda (x) (tag (list x))))
  'done)

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
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
       (lambda (x) (tag (list x))))
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
          (attach-tag 'complex x)))))
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
    (value (attach-tag 'polynomial x)))
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

(put 'add 'drop #t)
(put 'sub 'drop #t)
(put 'mul 'drop #t)
(put 'div 'drop #t)

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
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "polys not in same var -- add-poly"
             (list p1 p2))))
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
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "polys not in same var -- mul-poly"
             (list p1 p2))))
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
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
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
  (put '=zero? '(polynomial)
       (lambda (p)
         (let loop ((term-list (term-list p)))
           (if (empty-termlist? term-list)
             #t
             (let ((first (first-term term-list))
                   (rest (rest-terms term-list)))
               (and (=zero? (coeff first))
                    (loop rest)))))))
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
  (put 'make 'term
       (lambda (order coeff) (make-term order coeff)))
  (put 'make 'term-list
       (lambda (terms) (make-term-list terms)))
  'done)

;; ;; representation of terms and term lists
;; ;; dense representation of term list
;; (define (adjoin-term term term-list)
;;   (if (=zero? (coeff term))
;;     term-list
;;     (let loop ((i (length term-list))
;;                (term-list term-list))
;;       (cond ((= i (order term)) (cons (coeff term) term-list))
;;             ((> i (order term)) (error "bad usage of adjoin-term"
;;                                        (list term term-list)))
;;             (else
;;               (loop (+ i 1) (cons 0 term-list)))))))
;; (define (the-empty-termlist) '())
;; (define (first-term term-list)
;;   (make-term
;;     (length (cdr term-list))
;;     (car term-list)))
;; (define (rest-terms term-list) (cdr term-list))
;; (define (empty-termlist? term-list) (null? term-list))
;; (define (make-term order coeff) (list order coeff))
;; (define (make-term-list terms)
;;   (if (null? terms)
;;     (the-empty-termlist)
;;     (adjoin-term (car terms)
;;                  (make-term-list (cdr terms)))))
;; (define (order term) (car term))
;; (define (coeff term) (cadr term))

(install-polynomial-package)
(install-tower-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (make-term order coeff)
  ((get 'make 'term) order coeff))

(define (make-term-list . terms)
  ((get 'make 'term-list) terms))

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


(pretty-print
  (mul poly1 poly2))
