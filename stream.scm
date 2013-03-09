(define true #t)
(define false #f)
(define (square x) (* x x))
(define the-empty-stream '())
(define stream-null? null?)
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (display x)
  (newline))
(define (cons-stream a b)
  (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (delay e)
  (lambda () e))
(define (force delayed-object)
  (delayed-object))
(define (delay e)
  (memo-proc (lambda () e)))
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? true)
               result)
        result))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))
(define (show x)
  (display-line x)
  x)
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)

;; sum is 0 0 210 210 210 210 210
;; result will be same if delay is implemented using memo-proc

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(define primes
  (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))
;; solution for 3.53
;; produce the stream of power 2
(define s (cons-stream 1 (add-streams s s)))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
;; solution for 3.54
(define factorials
  (cons-stream 1 (mul-streams (integers-starting-from 2)
                              factorials)))
;; solution for 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (stream-map (lambda (x) (+ x (stream-car stream)))
                           (partial-sums (stream-cdr stream)))))

;; solution for 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (merge (scale-stream S 2)
                                       (scale-stream S 3))
                                (scale-stream S 5))))

;; solution for 3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

;; (expand 1 7 10) produces 1 4 2 8 5 7 1 4 2 8 5 7
;; (expand 3 8 10) produces 3 7 5 0 0 0
;; this procedure prodcues the digits of real number (/ (* num radix) den)

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series coeff-stream)
  (div-streams coeff-stream integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (let ((cars1 (stream-car s1))
        (cars2 (stream-car s2))
        (cdrs1 (stream-cdr s1))
        (cdrs2 (stream-cdr s2)))
    (cons-stream (* cars1 cars2)
                 (add-streams (stream-map (lambda (c) (* cars1 c))
                                          cdrs2)
                              (stream-map (lambda (c) (* cars2 c))
                                          cdrs1)
                              (cons-stream 0
                                           (mul-series cdrs1 cdrs2))))))

(define (invert-unit-series series)
  (cons-stream 1
               (stream-map - (mul-series (stream-cdr series)
                                         (invert-unit-series series)))))

(define (div-series s1 s2)
  (mul-series s1
              (invert-unit-series s2)))

(define tangent-series
  (div-series sine-series
              cosine-series))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit stream tolerance)
  (if (< (abs (- (stream-car stream)
                 (stream-car (stream-cdr stream))))
         tolerance
         (stream-car (stream-cdr stream)))
    (stream-limit (stream-cdr stream) tolerance)))

;; lot for log of two
(define (lot-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (lot-summands (+ n 1)))))

(define lot-stream
  (partial-sums (pi-summands 1)))

(display-stream lot-stream)

;; solution for 3.66
;; paris are placed into the stream ordered by column first, then by
;; row.
;; there are 4950 pairs precede the pair (1,100)
;; there are 5048 pairs precede the pair (99,100)
;; there are 5049 pairs precede the pair (100,100)
;;
;; there are C*(C-1)/2+R-1 pairs precede the pair (R,C)

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

;; solution for 3.67

(define (big-first-pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave (stream-map (lambda (x) (list x (stream-car t)))
                            (stream-cdr s))
                (big-first-pairs (stream-cdr s) (stream-cdr t)))))

(define all-integers-pairs
  (interleave (pairs integers integers)
              (big-first-pairs (integers-starting-from 2)
                               integers)))

(define pythagorean-triples (stream-filter (lambda (x)
                                             (= (square (caddr x))
                                                (+ (square (cadr x))
                                                   (square (car x)))))
                                           (triples integers
                                                    integers
                                                    integers)))

(define (triples s t u)
  (stream-map (lambda (x)
                (list (car x)
                      (cadr x)
                      (cadddr x)))
              (stream-filter
                (lambda (x)
                  (= (cadr x)
                     (caddr x)))
                (stream-map
                  (lambda (x y)
                    (append x y))
                  (pairs s t)
                  (pairs t u)))))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (pairs t (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
