(define-module (sicp utils))

(export
  sum
  fixed-point
  average-damp
  newtons-method
  square
  average
  repeated
  fexpt
  log2x)

(define (log2x x)
  (logNx x 2))

(define (logNx x n)
  (/ (log x) (log n)))

(define (sum lst)
  ;; sum a list from last element to first element
  ;; return the sum list
  ;; (1 2 3) will be (6 5 3)
  (define (sum-lst lst res)
    ;; add the first element of two lists
    ;; lst can't be '()
    ;; if res is '() return (car lst)
    ;; otherwise return the sume of them
    (define (add lst res)
      (if (null? res)
        (car lst)
        (+ (car lst) (car res))))
    (if (null? lst)
      res
      (sum-lst
        (cdr lst)
        (append (list (add lst res)) res))))
  (sum-lst (reverse lst) '()))

;;(define tolerance 0.00001)
(define tolerance 1e-9)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average . args)
  (define (sum lst res)
    (if (null? lst)
      res
      (sum
        (cdr lst)
        (+ res (car lst)))))
  (if (null? args)
    0)
  (/ (sum args 0.0) (length args)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square x) (* x x))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (repeated f n)
  (lambda (x)
    (define (iter x j)
      (if (> j n)
        x
        (iter (f x) (+ j 1))))
    (iter x 1)))

;; iterative process
(define (fexpt b n)
  (define (iter a b n)
    ;; invariant quantity is (* a (expt b n))
    (cond ((zero? n) a)
          ((odd? n) (iter (* a b) b (1- n)))
          ((even? n) (iter a (square b) (/ n 2)))))
  (iter 1 b n))
