(define (make-accumulator init)
  (let ((sum init))
    (lambda (n)
      (set! sum (+ sum n))
      sum)))
(define a (make-accumulator 5))
(a 10)
(a 10)

(define (make-monitored f)
  (let ((counter 0))
    (lambda (a)
      (cond ((eq? a 'how-many-calls?) counter)
            ((eq? a 'reset-count)
             (set! counter 0)
             counter)
            (else
              (set! counter (+ counter 1))
              (f a))))))
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)

(define (make-account balance account-password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (joint joint-password)
    (make-dispatch joint-password 0))
  (define (call-the-cops)
    (lambda (m)
      "dial 911"))
  (define (make-dispatch dispatch-password counter)
    (lambda (password m)
      (if (eq? password dispatch-password)
        (begin
          (set! counter 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'joint) joint)
                (else (error "unknown request -- make-account"
                             m))))
        (begin
          (set! counter (+ counter 1))
          (if (> counter 7)
            (call-the-cops)
            (lambda (m)
              "incorrect password"))))))
  (make-dispatch account-password 0))

(define acc (make-account 100 'secret-password))
(display ((acc 'secret-password 'withdraw) 40))
(newline)
(display ((acc 'some-other-password 'deposit) 50))
(newline)
(define (acc-n n)
  (let loop ((i 1))
    (if (>= i n)
      ((acc 'some-other-password 'deposit) 50)
      (begin
        ((acc 'some-other-password 'deposit) 50)
        (loop (+ i 1))))))
(define (make-joint account account-password joint-password)
  ((account account-password 'joint) joint-password))
(define peter-acc (make-account 100 'open-sesame ))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

;; exercise 3.9
;; iterative version has one extra frame for factorial
;; each frame in iterative version has three variable bindings
;; instead of one variable binding in recursive version.
;;
;; exercise 3.10
;; with a let in between, one extra frame with initial-amount binds to 100
;; is created when W1 and W2 is defined.
;; when (W1 50) is applied, only the frame contains bindings for balance
;; to 100 is altered.
;;
;; exercise 3.10
;; local state of acc is saved in the frame of applying make-account.
;; global env is shared between acc and acc2.
