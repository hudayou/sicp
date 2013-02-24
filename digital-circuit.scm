(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and a1 a2)
  (cond ((and (= a1 0) (= a2 0)) 0)
        ((and (= a1 0) (= a2 1)) 0)
        ((and (= a1 1) (= a2 0)) 0)
        ((and (= a1 1) (= a2 1)) 1)
        (else (error "invalid signal" (list a1 a2)))))
;; or-gate as primitive
(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)
(define (logical-or o1 o2)
  (cond ((and (= o1 1) (= o2 1)) 1)
        ((and (= o1 l) (= o2 0)) 1)
        ((and (= o1 0) (= o2 1)) 1)
        ((and (= o1 0) (= o2 0)) 0)
        (else (error "invalid signal" (list o1 o2)))))
;; or-gate as compound
;; or-gate-delay is 2 * inverter-delay + and-gate-delay
(define (or-gate o1 o2 output)
  (let  ((i1 (make-wire))
         (i2 (make-wire))
         (a (make-wire)))
    (inverter o1 i1)
    (inverter o2 i2)
    (and-gate i1 i2 a)
    (inverter a output)))
;; for half-adder,
;; the delay to get the s is:
;; (+ (max (+ and-gate-delay inverter-delay)
;;         or-gate-delay)
;;    and-gate-delay)
;; the delay to get the c is:
;; and-gate-delay
;;
;; for full-adder,
;; the delay to get the s is:
;; (* 2 half-adder-s-delay)
;; the delay to get the c is:
;; (+ half-adder-s-delay half-adder-c-delay or-gate-delay)
;;
;; for ripple-carry-adder,
;; the delay to get the c-n is:
;; (* (- n 1) full-adder-c-delay)
;; the delay to get the s-n is:
;; (+ c-n-1 full-adder-s-delay)
(define (ripple-carry-adder list-a list-b list-s c)
  (if (null? (cdr list-a))
    (let ((c-in (make-wire)))
      (set-signal! c-in 0)
      (full-adder (car list-a)
                  (car list-b)
                  c-in
                  (car list-s)
                  c))
    (let ((c-out (make-wire)))
      (ripple-carry-adder (cdr list-a)
                          (cdr list-b)
                          (cdr list-s)
                          c-out)
      (full-adder (car list-a)
                  (car list-b)
                  c-out
                  (car list-s)
                  c))))
