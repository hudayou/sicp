(use-modules (ice-9 streams))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(define (stream-display s)
  (stream-for-each display-line s))
(define (display-line x)
  (display x)
  (newline))
(define (stream-enumerate-interval low high)
  (make-stream (lambda (state)
                 (if (> state high)
                   '()
                   (cons state
                         (+ state 1))))
               low))
(define (stream-filter pred stream)
  (make-stream (lambda (state)
                 (if (stream-null? state)
                   '()
                   (if (pred (stream-car state))
                     (cons (stream-car state) (stream-cdr state))
                     (if (stream-null? (stream-cdr state))
                       '()
                       (cons (stream-car (stream-cdr state))
                             (stream-cdr (stream-cdr state)))))))
               stream))
(define (integers-starting-from n)
  (make-stream (lambda (state) (cons state (+ state 1)))
               n))
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
(define (fibgen a b)
  (make-stream (lambda (state)
                 (cons (car state)
                       (cons (cdr state)
                             (+ (car state)
                                (cdr state)))))
               (cons a b)))
(define fibs (fibgen 0 1))
(define (sieve stream)
  (make-stream (lambda (state)
                 (cons (stream-car state)
                       (stream-filter
                         (lambda (x)
                           (not (divisible? x (stream-car state))))
                         (stream-cdr state))))
               stream))
(define primes (sieve (integers-starting-from 2)))
(define (xes x)
  (make-stream (lambda (state)
                 (cons state
                       state))
               x))
(define ones
  (xes 1))
(define (add-stream s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (xble x)
  (make-stream (lambda (state)
                 (cons state
                       (* x state)))
               1))
(define double (xble 2))
(define factorials
  (make-stream (lambda (state)
                 (cons (cdr state)
                       (cons (+ (car state) 1)
                             (* (+ (car state) 1)
                                (cdr state)))))
               (cons 1 1)))
(define (partial-sums stream)
  (make-stream (lambda (state)
                 (cons (cdr state)
                       (cons (stream-cdr (car state))
                             (+ (stream-car (car state))
                                (cdr state)))))
               (cons (stream-cdr stream) (stream-car stream))))

(define enumerate-hamming-integers
  ;; state is list
  ;; (hamming-integer
  ;;   (scale-stream integers 2)
  ;;   (scale-stream integers 3)
  ;;   (scale-stream integers 5))
  (make-stream (lambda (state)
                 (cons (car state)
                       (let ((twos (cadr state))
                             (threes (caddr state))
                             (fives (cadddr state)))
                         (let ((first-two (stream-car twos))
                               (first-three (stream-car threes))
                               (first-five (stream-car fives)))
                           (cond ((and (< first-two first-three)
                                       (< first-two first-five))
                                  (list first-two
                                        (stream-cdr twos)
                                        threes
                                        fives))
                                 ((and (< first-three first-two)
                                       (< first-three first-five))
                                  (list first-three
                                        twos
                                        (stream-cdr threes)
                                        fives))
                                 ((and (< first-five first-two)
                                       (< first-five first-three))
                                  (list first-five
                                        twos
                                        threes
                                        (stream-cdr fives)))
                                 ((= first-two first-three first-five)
                                  (list first-two
                                        (stream-cdr twos)
                                        (stream-cdr threes)
                                        (stream-cdr fives)))
                                 ((= first-two first-three)
                                  (list first-two
                                        (stream-cdr twos)
                                        (stream-cdr threes)
                                        fives))
                                 ((= first-three first-five)
                                  (list first-three
                                        twos
                                        (stream-cdr threes)
                                        (stream-cdr fives)))
                                 ((= first-two first-five)
                                  (list first-five
                                        (stream-cdr twos)
                                        threes
                                        (stream-cdr fives))))))))
               (list 1
                     (scale-stream integers 2)
                     (scale-stream integers 3)
                     (scale-stream integers 5))))

(define (one-shot-stream bullet)
  (let ((shot #f))
    (make-stream (lambda (state)
                   (if shot
                     '()
                     (begin
                       (set! shot #t)
                       (cons state '()))))
                     bullet)))

(define enumerate-hamming-integers
  (make-stream (lambda (state)
                 (cons (stream-car state)
                         (merge-stream (scale-stream state 2)
                                       (scale-stream state 3))
                         ))
               (one-shot-stream 1)))

(define (merge-stream stream1 stream2)
  (make-stream
    (lambda (state)
      (let ((s1 (car state))
            (s2 (cdr state)))
        (cond ((and (stream-null? s1)
                    (stream-null? s2))
               (cons '() '()))
              ((stream-null? s1) (cons (stream-car s2)
                                       (cons s1 (stream-cdr s2))))
              ((stream-null? s2) (cons (stream-car s1)
                                       (cons (stream-cdr s1) s2)))
              (else
                (let ((s1car (stream-car s1))
                      (s2car (stream-car s2)))
                  (cond ((< s1car s2car)
                         (cons s1car (cons (stream-cdr s1) s2)))
                        ((> s1car s2car)
                         (cons s2car (cons s1 (stream-cdr s2))))
                        (else
                          (cons s1car (cons (stream-cdr s1)
                                            (stream-cdr s2))))))))))
    (cons stream1 stream2)))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series coeff-stream)
  (div-streams coeff-stream integers))
