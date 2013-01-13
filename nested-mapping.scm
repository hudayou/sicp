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

(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-pair n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
    ; empty set?
    (list nil)
    ; sequence containing empty set
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (unique-pairs n)
  (if (< n 2)
    nil
    (append (map (lambda (x)
                   (list n x))
                 (enumerate-interval 1 (- n 1)))
            (unique-pairs (- n 1)))))

(define (unique-triples n)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j)
                 (map (lambda (k) (list i j k))
                      (enumerate-interval 1 (- j 1))))
               (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (make-triple-sum x s)
  (let ((carx (car x))
        (cadrx (cadr x))
        (caddrx (caddr x)))
    (list carx cadrx caddrx s)))

(define (fixed-sum? x s)
  (let ((carx (car x))
        (cadrx (cadr x))
        (caddrx (caddr x)))
    (= s (+ carx cadrx caddrx))))

(define (fixed-sum-triples n s)
  (map (lambda (x) (make-triple-sum x s))
       (filter (lambda (x) (fixed-sum? x s)) (unique-triples n))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (safe? k positions)
  (if (= k 1)
    #t
    (let ((cark (car positions))
          (cdrk (cdr positions)))
      (accumulate (lambda (position safe)
                    (let ((caark (car cark))
                          (cadrk (cadr cark))
                          (caarp (car position))
                          (cadrp (cadr position)))
                      (and safe
                           (not (= caark caarp))
                           (not (= (abs (- caark caarp))
                                   (abs (- cadrk cadrp)))))))
                  #t
                  cdrk))))

;;
;; T(n) = nT(n-1) + O(n)
;;      = O(n/e^n)
;; compared to
;; T(n) = T(n-1) + O(n)
;;      = O(n^2)
;;
;;(define (queens board-size)
;;  (define (queen-cols k)
;;    (if (= k 0)
;;      (list empty-board)
;;      (filter
;;        (lambda (positions) (safe? k positions))
;;        (flatmap
;;          (lambda (new-row)
;;            (map (lambda (rest-of-queens)
;;                   (adjoin-position new-row k rest-of-queens))
;;                 (queen-cols (- k 1))))
;;          (enumerate-interval 1 board-size)))))
;;  (queen-cols board-size))
