;; (list 1 (list 2 (list 3 4)))
;; (1 (2 (3 4)))
;;
;; x -> x -> x -> 4
;; |    |    |
;; v    v    v
;; 1    2    3
;;   (1 (2 (3 4)))
;;  1             (2 (3 4))
;;               2         (3 4)
;;                        3     4

(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))

(car (car '((7))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

(define x (list 1 2 3))
(define y (list 4 5 6))


(equal? (list '(1 2 3) '(4 5 6)) '((1 2 3) (4 5 6)))
(equal? (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(equal? (cons '(1 2 3) '(4 5 6)) '((1 2 3) 4 5 6))

;;(define (deep-reverse lst)
;;  (define (iter l r)
;;    (if (null? l)
;;      r
;;      (if (pair? (car l))
;;        (iter (cdr l) (cons (iter (car l) '()) r))
;;        (iter (cdr l) (cons (car l) r)))))
;;  (iter lst '()))

(define nil '())

(define (deep-reverse lst)
  (cond ((null? lst)
         nil)
        ((pair? (car lst))
         (append (reverse (cdr lst))
                 (list (reverse (car lst)))))
        (else
          (append (reverse (cdr lst))
                  (list (car lst))))))

(define (fringe lst)
  (cond ((null? lst)
         nil)
        ((not (pair? (car lst)))
          (cons (car lst) (fringe (cdr lst))))
        (else
         (append (fringe (car lst)) (fringe (cdr lst))))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (number? structure)
      structure
      (+ (total-weight structure)))))

;; ((2 ((4 9) (3 12))) (6 7))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced-branch? branch)
  (let ((structure (branch-structure branch)))
    (if (number? structure)
      #t
      (balanced-mobile? structure))))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced-mobile? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (branch-torque left)
            (branch-torque right))
         (balanced-branch? left)
         (balanced-branch? right))))
