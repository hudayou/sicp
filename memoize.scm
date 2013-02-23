(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))
(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
;; the following procedure will be faster since memo-fib(n-2) is caculated
;; before memo-fib(n-1)
(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 2))
                            (memo-fib (- n 1))))))))
;; f(n) = f(n-1) + f(n-2)
;; f(n) = f(n-1) + O(1)
;; f(n) = O(n)
;; since memo-fib's enclosing frame is a frame contains binding to table,
;; which enclosing frame is a frame contains binding to f, which enclosing
;; frame is the global frame.
;; but fib's enclosing frame is the global frame.
;; so each time memo-fib is applied, it's result will be remembered in table.
;; and to be used next time.
;;(define memo-fib
;;  (memoize fib))
