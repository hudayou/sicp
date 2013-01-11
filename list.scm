(define (last-pair  lst)
  (if (null? lst)
    '()
    (if (null? (cdr lst))
      lst
      (last-pair (cdr lst)))))

(define (reverse lst)
  (define (iter l r)
    (if (null? l)
      r
      (iter (cdr l) (cons (car l) r))))
  (iter lst '()))

(define (same-parity init . more)
  (define pred? even?)
  (define (iter lst res)
    (if (null? lst)
      res
      (let ((head (car lst))
            (pi (pred? init))
            (tail (cdr lst)))
        (if (eq? pi (pred? head))
          (iter tail (append res (list head)))
          (iter tail res)))))
  (iter more (list init)))

(define (filter pred? lst)
  (define (recurse res lst)
    (if (null? lst)
      res
      (if (pred? (car lst))
        (recurse (append res (list (car lst))) (cdr lst))
        (recurse res (cdr lst)))))
  (recurse '() lst))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define no-more? null?)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

(define (square x) (* x x))

(define nil '())

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(define (foreach proc items)
  (if (null? items)
    nil
    (begin (proc (car items))
           (foreach proc (cdr items)))))

(foreach (lambda (x) (display x) (newline))
         (list 57 321 88))
