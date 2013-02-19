(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
(define w (append! x y))
w
(cdr x)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (circular-list? x)
  (define (circle? x y)
    (cond ((null? (cdr x)) #f)
          ((eq? (cdr x) y) #t)
          (else
            (circle? (cdr x) y))))
  (circle? x x))

(define z (make-cycle (list 'a 'b 'c)))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
(define u (mystery w))
;; notice the sharing of data between u, v and w

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
z1
(set-to-wow! z1)
z2
(set-to-wow! z2)
(eq? (car z1) (cdr z1))
(eq? (car z2) (cdr z2))

;;  Ben Bitdiddle's pair counter
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))
(define three-pairs '(a b c))
(define a (list 'a))
(define a (cons 'a 'a))
(define four-pairs (list a a))
(define b (cons a a))
(define seven-pairs (cons b b))
(define inf-pairs (make-cycle '(a b c)))
