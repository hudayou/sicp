;; How many different ways can we make change of $1.00,
;; given half-dollars(50), quarters(25), dimes(10), nickels(5), pennies(1)?

;; linear recursive  process
;; (cc 11 5) <- (+ (cc 11 4) (cc -39 5))
;; (cc 11 4) <- (+ (cc 11 3) (cc -14 4))
;; (cc 11 3) <- (+ (cc 11 2) (cc 1 3))
;; (cc 11 2) <- (+ (cc 11 1) (cc 6 2))
;; (cc 11 1) <- (+ (cc 11 0) (cc 10 1))
;; (cc 10 1) <- (+ (cc 10 0) (cc 9 1))
;; (cc 9 1) <- (+ (cc 9 0) (cc 8 1))
;; (cc 8 1) <- (+ (cc 8 0) (cc 7 1))
;; (cc 7 1) <- (+ (cc 7 0) (cc 6 1))
;; (cc 6 1) <- (+ (cc 6 0) (cc 5 1))
;; (cc 5 1) <- (+ (cc 5 0) (cc 4 1))
;; (cc 4 1) <- (+ (cc 4 0) (cc 3 1))
;; (cc 3 1) <- (+ (cc 3 0) (cc 2 1))
;; (cc 2 1) <- (+ (cc 2 0) (cc 1 1))
;; !! (cc 1 1) <- (+ (cc 1 0) (cc 0 1))
;; (cc 1 3) <- (+ (cc 1 2) (cc -9 3))
;; (cc 1 2) <- (+ (cc 1 1) (cc -4 2))
;; !! (cc 1 1) <- (+ (cc 1 0) (cc 0 1))
;; (cc 6 2) <- (+ (cc 6 1) (cc 1 2))
;; (cc 6 1) <- (+ (cc 6 0) (cc 1 1))
;; !! (cc 1 1) <- (+ (cc 1 0) (cc 0 1))
;; (cc 1 2) <- (+ (cc 1 1) (cc -3 2))
;; !! (cc 1 1) <- (+ (cc 1 0) (cc 0 1))
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; iterative process
;; a <- c ways of count change using all n kinds of coin
;; a+1 <- c + the number of ways to change amount a+1 using all but the first
;;            kind of coin
;;
;; use (a + 1) x n list of lists to lookup previous result
;;
;; coin in the following order (order . denomination)
;; ((0 . 1) (1 . 5) (2 . 10) (3 . 25) (4 . 50))
;; take amount 5 and 5 kinds of coin as example:
;; ((0 0 0 0 0) (1 0 0 0 0) (1 0 0 0 0) (1 0 0 0 0) (1 0 0 0 0) (2 1 0 0 0))
;; 
;; take 0th list (0 0 0 0 0) as example:
;; the first element means ways of change 0 in all 5 kind of coins
;; (1 5 10 25 50)
;; the second element means ways of change 0 in all 4 kinds of coins
;; (5 10 25 50)
;; the third element means ways of change 0 in all 3 kinds of coins
;; (10 25 50)
;; the fourth element means ways of change 0 in all 2 kinds of coins
;; (25 50)
;; the fifth element means ways of change 0 in all 1 kinds of coins
;; (50)

(use-modules (sicp utils))

(define (iter-count-change amount)
  (define list-of-coins
    '((0 . 1) (1 . 5) (2 . 10) (3 . 25) (4 . 50)))
  (define no-way
    '(0 0 0 0 0))
  (define (change-coins amount ways-of-change)
    (if (zero? amount)
      no-way
      (map-in-order
        (lambda (c)
          (let ((d (- amount (cdr c))))
            (cond ((< d 0) 0)
                  ((= d 0) 1)
                  (else (list-ref
                          (list-ref ways-of-change d)
                          (car c))))))
        list-of-coins)))
  (define (iter-cc inc amount ways-of-change)
    (if (> inc amount)
      (car (car (reverse ways-of-change)))
      ;;ways-of-change
      (iter-cc
        (1+ inc)
        amount
        (append ways-of-change
                (list
                  (sum (change-coins inc ways-of-change)))))))
  (if (< amount 1)
    no-way
    (iter-cc 0 amount '())))
