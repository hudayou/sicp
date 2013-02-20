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
;;(define three-pairs '(a b c))
(define three-pairs (list 'a 'b 'c))
(define a (list 'a))
(define a (cons 'a 'a))
(define four-pairs (list a a))
(define b (cons a a))
(define seven-pairs (cons b b))
;;(define inf-pairs (make-cycle '(a b c)))
(define inf-pairs (make-cycle (list 'a 'b 'c)))

;; Correct pair counter
(define (count x pairs)
  (if (not (pair? x))
    (cons 0 pairs)
    (if (memq x pairs)
      (cons 0 pairs)
      (let ((n-pairs (cons x pairs)))
        (let ((car-result (count (car x) n-pairs)))
          (let ((car-count (car car-result))
                (car-pairs (cdr car-result)))
            (let ((cdr-result (count (cdr x) car-pairs)))
              (let ((cdr-count (car cdr-result))
                    (cdr-pairs (cdr cdr-result)))
                (cons (+ car-count cdr-count 1)
                      cdr-pairs)))))))))
(define (count-pairs x)
  (car (count x '())))

(define c (list 1 2 3 4))
(set-cdr! (last-pair c) (cddr c))

(define (circle? x y)
  (cond ((null? (cdr x)) #f)
        ((memq (cdr x) y) #t)
        (else
          (circle? (cdr x) (cons (cdr x) y)))))
(define (circular-list? x)
  (circle? x (cons x '())))

(define (circular-list? x)
  (cond ((not (pair? x)) #f)
        ((not (pair? (cdr x))) #f)
        (else
          (circle? (cons (cdr x) (cddr x))))))
(define (circle? x)
  (cond ((eq? (car x) (cdr x)) #t)
        ((null? (cdar x)) #f)
        ((null? (cddr x)) #f)
        ((null? (cdddr x)) #f)
        (else
            (set-car! x (cdar x))
            (set-cdr! x (cdddr x))
            (circle? x))))

(define o '(o))
(set-cdr! o o)

;; (define (cons x y)
;;   (define (set-x! v) (set! x v))
;;   (define (set-y! v) (set! y v))
;;   (define (dispatch m)
;;     (cond ((eq? m 'car) x)
;;           ((eq? m 'cdr) y)
;;           ((eq? m 'set-car!) set-x!)
;;           ((eq? m 'set-cdr!) set-y!)
;;           (else (error "Undefined operation -- CONS" m))))
;;   dispatch)
;; (define (car z) (z 'car))
;; (define (cdr z) (z 'cdr))
;; (define (set-car! z new-value)
;;   ((z 'set-car!) new-value)
;;   z)
;; (define (set-cdr! z new-value)
;;   ((z 'set-cdr!) new-value)
;;   z)
;; (define x (cons 1 2))
;; (define z (cons x x))
;; (set-car! (cdr z) 17)
;; (car x)
;;
;; cdr z returns the procedure object x, calling set-car! on x,
;; will set the x binding in x's enclosing frame to be 17.
