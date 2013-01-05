(define (square  x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ;;(else (find-divisor n (+ test-divisor 1)))))
        ;; next statetment is two times faster since input grows two
        ;; times faster
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define runtime get-internal-real-time)
(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime))
  (newline))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes low high)
  (define (search low high)
    (if (> low high)
      #f
      (begin (timed-prime-test low)
             (search (+ low 2) high))))
  (cond ((<= low 1) #f)
        ((< high low) #f)
        ((odd? low) (search low high))
        (else (search (1+ low) high))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (1+ (random (1- n)))))

(define (ftest n)
  (define (try-it a b n)
    (cond ((not b) b)
          ((< a n)
           (try-it (1+ a) (= (expmod a n n) a) n))
          (else
            b)))
  (try-it 2 #t n))

;; Numbers that fool the Fermat test are called Carmichael numbers, and little
;; is known about them other than that they are extremely rare.  There are 255
;; Carmichael numbers below 100,000,000. The smallest few are 561, 1105, 1729,
;; 2465, 2821, and 6601. In testing primality of very large numbers chosen at
;; random, the chance of stumbling upon a value that fools the Fermat test is
;; less than the chance that cosmic radiation will cause the computer to make
;; an error in carrying out a ``correct'' algorithm. Considering an algorithm
;; to be inadequate for the first reason but not for the second illustrates the
;; difference between mathematics and engineering.
(define carmichael-numbers '(561 1105 1729 2465 2821 6601))

(define (carmic-test)
  (map ftest carmichael-numbers))

(define (miller-rabin-test n)
  (define (expmod base exp m)
    ;; (=
    ;;   (remainder (* x y) n)
    ;;   (remainder (* (remainder x n) (remainder y n)) n))
    (define (nontrivial? x)
      ;; (=
      ;;   (remainder (* x x) n) 1)
      (cond ((= x 1) #f)
            ((= x (1- m)) #f)
            (else (= (remainder (square x) m) 1))))
    (cond ((= exp 0) 1)
          ((even? exp)
           (if (nontrivial? (expmod base (/ exp 2) m))
             0
             (remainder (square (expmod base (/ exp 2) m))
                        m)))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))
  ;;(define (iter)
  (define (try-it a)
    (= (expmod a (1- n) n) 1))
  (cond ((< n 2) #f)
        ((= n 2) #t)
        ((even? n) #f)
        ((odd? n) (try-it (1+ (random (1- n)))))))
