;; An interval tree implemented as a treap

(define nil '())

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

(define (interval-less? interval1 interval2)
  (let ((low1 (interval-low interval1))
        (low2 (interval-low interval2)))
    (< low1 low2)))

(define (interval-equal? interval1 interval2)
  (equal? interval1 interval2))

;; An node is a list
;; (key priority value)
(define (make-node key priority value)
  (list key priority value))

(define (node-key node) (car node))

(define (node-priority node) (cadr node))

(define (node-value node) (caddr node))

(define (node-overlaps-interval? node interval)
  (interval-overlaps? (node-key node) interval))

;; A tree is list
;; (node left right)
(define (make-tree node left right)
  (list node left right))

(define (tree-node tree) (car tree))

(define (tree-left tree) (cadr tree))

(define (tree-right tree) (caddr tree))

(define (interval-search interval tree)
  (define (tree-overlaps-interval? tree interval)
    (if (null? tree)
      #f
      (let ((value (node-value (tree-node tree)))
            (low (interval-low interval)))
        (>= value low))))
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

(define (random-priority)
  (random:uniform))

(define (time-priority)
  (let ((time (gettimeofday)))
    (let ((second (car time))
          (microsecond (cdr time)))
      (- (+ (* second 1000000) microsecond)))))

(define (interval-insert interval tree)
  (define (priority-less? tree1 tree2)
    (let ((priority1 (node-priority (tree-node tree1)))
          (priority2 (node-priority (tree-node tree2))))
      (< priority1 priority2)))
  (define (tree-value tree)
    (if (null? tree)
      -inf.0
      (node-value (tree-node tree))))
  (define (update-node-value node value)
    (let ((key (node-key node))
          (priority (node-priority node)))
      (make-node key priority value)))
  (define (update-tree-value tree)
    (if (null? tree)
      tree
      (let ((node (tree-node tree))
            (left (tree-left tree))
            (right (tree-right tree)))
        (make-tree
          (update-node-value node
                             (max (tree-value tree)
                                  (tree-value left)
                                  (tree-value right)))
          left
          right))))
  (define (fixup tree)
    (let ((node (tree-node tree))
          (left (tree-left tree))
          (right (tree-right tree)))
      (cond ((and (null? left) (null? right))
             tree)
            ((null? left)
             (if (priority-less? tree right)
               (let ((left-of-right (tree-left right))
                     (right-of-right (tree-right right))
                     (node-of-right (tree-node right)))
                 (update-tree-value
                   (make-tree
                     node-of-right
                     (update-tree-value
                       (make-tree
                         node
                         nil
                         left-of-right))
                     right-of-right)))
               (update-tree-value tree)))
            ((null? right)
             (if (priority-less? tree left)
               (let ((left-of-left (tree-left left))
                     (right-of-left (tree-right left))
                     (node-of-left (tree-node left)))
                 (update-tree-value
                   (make-tree
                     node-of-left
                     left-of-left
                     (update-tree-value
                       (make-tree
                         node
                         right-of-left
                         nil)))))
               (update-tree-value tree)))
            (else
              (let ((left-of-right (tree-left right))
                    (right-of-right (tree-right right))
                    (node-of-right (tree-node right))
                    (left-of-left (tree-left left))
                    (right-of-left (tree-right left))
                    (node-of-left (tree-node left)))
                (cond ((priority-less? tree left)
                       (update-tree-value
                         (make-tree
                           node-of-left
                           left-of-left
                           (update-tree-value
                             (make-tree
                               node
                               right-of-left
                               right)))))
                      ((priority-less? tree right)
                       (update-tree-value
                         (make-tree
                           node-of-right
                           (update-tree-value
                             (make-tree
                               node
                               left
                               left-of-right))
                           right-of-right)))
                      (else
                        (update-tree-value tree))))))))
  (define (insert interval tree)
    (if (null? tree)
      (make-tree
        (make-node interval
                   (random-priority)
                   (interval-high interval))
        nil
        nil)
      (let ((node (tree-node tree))
            (left (tree-left tree))
            (right (tree-right tree)))
        (let ((key (node-key node)))
          (cond ((interval-less? interval key)
                 (fixup
                   (make-tree node
                              (insert interval left)
                              right)))
                ;; swallow duplicate insertions
                ((interval-equal? interval key)
                 tree)
                ;; flip a coin to decide to go left or right,
                ;; if low part of interval and key equal?
                (else
                  (fixup
                    (make-tree node
                               left
                               (insert interval right)))))))))
  (insert interval tree))

(use-modules (ice-9 pretty-print))

(define tree-1 (interval-insert
                 '(2 . 5)
                 (interval-insert
                   '(4 . 7)
                   (interval-insert
                     '(6 . 9)
                     (interval-insert
                       '(8 . 11)
                       (interval-insert
                         '(10 . 13)
                         (interval-insert
                           '(12 . 15)
                           '())))))))

(define tree-2 (interval-insert
                 '(4 . 5)
                 (interval-insert
                   '(4 . 7)
                   (interval-insert
                     '(4 . 9)
                     (interval-insert
                       '(4 . 11)
                       (interval-insert
                         '(4 . 13)
                         (interval-insert
                           '(4 . 15)
                           '())))))))
(pretty-print tree-1)
(pretty-print tree-2)
