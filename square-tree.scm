(define nil '())

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items)) (map proc (cdr items)))))

(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((pair? tree)
         (cons (tree-map proc (car tree))
               (tree-map proc (cdr tree))))
        (else
          (proc tree))))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree)
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))
        (else
          (square tree))))

(define (square-tree tree)
  (map
    (lambda (tree)
      (if (pair? tree)
        (square-tree tree)
        (square tree)))
    tree))

(define (square-tree tree)
  (tree-map square tree))

(define (test-square-tree) (square-tree
                             (list 1
                                   (list 2 (list 3 4) 5)
                                   (list 6 7))))

;; cdring down the list
;; until we reach the base case,
;; which is an empty list, whose subset is an list of empty list.
;; then for every subset of the cdr
;; cons the car of the original list with them.
;; Finally we get the subsets of the whole list.
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      ;;(display rest)
      ;;(newline)
      (append rest
              (map
                (lambda (x)
                  (cons (car s) x))
                rest)))))
