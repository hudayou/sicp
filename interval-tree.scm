;; An interval tree is an augmented red-black tree which supports operations
;; on dynamic sets of intervals.

;; An interval is a pair
;; (low . high)
(define (make-interval low high) (cons low high))

(define (interval-low interval) (car interval))

(define (interval-high interval) (cdr interval))

(define (interval-overlaps? interval1 interval2)
  (let ((low1 (interval-low interval1))
        (high1 (interval-high interval1))
        (low2 (interval-low interval2))
        (high2 (interval-high interval2)))
    (and (<= low1 high2)
         (<= low2 high1))))

;; An node is a list
;; (color interval highest)
(define (make-node color interval highest)
  (list color interval highest))

(define (node-color node) (car node))

(define (node-interval node) (cadr node))

(define (node-highest node) (caddr node))

(define (node-key node)
  (interval-low
    (node-interval node)))

(define (node-overlaps-interval? node interval)
  (interval-overlaps? (node-interval node) interval))

;; A tree is list
;; (node left right)
(define (make-tree node left right)
  (list node left right))

(define (tree-node tree) (car tree))

(define (tree-left tree) (cadr tree))

(define (tree-right tree) (caddr tree))

(define (tree-overlaps-interval? tree interval)
  (if (null? tree)
    #f
    (let ((highest (node-highest (tree-node tree)))
          (low (interval-low interval)))
      (>= highest low))))

(define (interval-search interval tree)
  (if (null? tree)
    #f
    (let ((node (tree-node tree))
          (left (tree-left tree))
          (right (tree-right tree)))
      (cond ((node-overlaps-interval? node interval)
             (node-interval node))
            ((tree-overlaps-interval? left interval)
             (interval-search interval left))
            (else
              (interval-search interval right))))))
