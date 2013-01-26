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

;; An node is a list
;; (key priority data value)
(define (make-node key priority data value)
  (list key priority data value))

(define (node-key node) (car node))

(define (node-priority node) (cadr node))

(define (node-data node) (caddr node))

(define (node-value node) (cadddr node))

;; A tree is list
;; (node left right)
(define (make-tree node left right)
  (list node left right))

(define (tree-node tree) (car tree))

(define (tree-left tree) (cadr tree))

(define (tree-right tree) (caddr tree))

(define (priority-less? tree1 tree2)
  (let ((priority1 (node-priority (tree-node tree1)))
        (priority2 (node-priority (tree-node tree2))))
    (< priority1 priority2)))

;; search, insert, delete on the hash tree

(define (hash-search hash tree)
  (if (null? tree)
    #f
    (let ((node (tree-node tree))
          (left (tree-left tree))
          (right (tree-right tree)))
      (let ((k (node-key node)))
        (cond ((< hash k)
               (hash-search hash left))
              ((= hash k)
               #t)
              (else
                (hash-search hash right)))))))

(define (hash-insert hash tree)
  (treap-insert
    hash
    (time-priority)
    nil
    nil
    tree
    <
    identity))

(define (hash-delete hash tree)
  (treap-delete
    hash
    tree
    <
    identity))

;; search, insert, delete on the interval tree

(define (node-overlaps-interval? node interval)
  (interval-overlaps? (node-key node) interval))

(define (tree-overlaps-interval? tree interval)
  (if (null? tree)
    #f
    (let ((data (node-data (tree-node tree)))
          (low (interval-low interval)))
      (>= data low))))

(define (update-tree-data tree)
  (define (tree-data tree)
    (if (null? tree)
      -inf.0
      (node-data (tree-node tree))))
  (define (update-node-data node data)
    (let ((key (node-key node))
          (priority (node-priority node))
          (value (node-value node)))
      (make-node key priority data value)))
  (if (null? tree)
    tree
    (let ((node (tree-node tree))
          (left (tree-left tree))
          (right (tree-right tree)))
      (make-tree
        (update-node-data node
                          (max (tree-data tree)
                               (tree-data left)
                               (tree-data right)))
        left
        right))))

(define (interval-search interval tree)
  (if (null? tree)
    #f
    (let ((node (tree-node tree))
          (left (tree-left tree))
          (right (tree-right tree)))
      (cond ((node-overlaps-interval? node interval)
             (node-value node))
            ((tree-overlaps-interval? left interval)
             (interval-search interval left))
            (else
              (interval-search interval right))))))

(define (interval-traverse-search interval tree)
  (define combine hash-insert)
  (define (search-to-result tree result)
    (if (null? tree)
      result
      (let ((node (tree-node tree))
            (left (tree-left tree))
            (right (tree-right tree)))
        (let ((value (node-value node)))
          (if (node-overlaps-interval? node interval)
            (if (tree-overlaps-interval? left interval)
              (search-to-result left
                                (combine value
                                         (search-to-result right result)))
              (search-to-result right (combine value result)))
            (if (tree-overlaps-interval? left interval)
              (search-to-result left
                                (search-to-result right result))
              (search-to-result right result)))))))
  (search-to-result tree nil))

(define (interval-insert interval value tree)
  (treap-insert
    interval
    (random-priority)
    (interval-high interval)
    value
    tree
    interval-less?
    update-tree-data))

(define (interval-delete interval tree)
  (treap-delete
    interval
    tree
    interval-less?
    update-tree-data))

;; random and time priority

(define (random-priority)
  (random:uniform))

(define (time-priority)
  (let ((time (gettimeofday)))
    (let ((second (car time))
          (microsecond (cdr time)))
      (- (+ (* second 1000000) microsecond)))))

;; insert and delete on the treap

(define (treap-insert key priority data value tree key-less? update-data)
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
                 (update-data
                   (make-tree
                     node-of-right
                     (update-data
                       (make-tree
                         node
                         nil
                         left-of-right))
                     right-of-right)))
               (update-data tree)))
            ((null? right)
             (if (priority-less? tree left)
               (let ((left-of-left (tree-left left))
                     (right-of-left (tree-right left))
                     (node-of-left (tree-node left)))
                 (update-data
                   (make-tree
                     node-of-left
                     left-of-left
                     (update-data
                       (make-tree
                         node
                         right-of-left
                         nil)))))
               (update-data tree)))
            (else
              (let ((left-of-right (tree-left right))
                    (right-of-right (tree-right right))
                    (node-of-right (tree-node right))
                    (left-of-left (tree-left left))
                    (right-of-left (tree-right left))
                    (node-of-left (tree-node left)))
                (cond ((priority-less? tree left)
                       (update-data
                         (make-tree
                           node-of-left
                           left-of-left
                           (update-data
                             (make-tree
                               node
                               right-of-left
                               right)))))
                      ((priority-less? tree right)
                       (update-data
                         (make-tree
                           node-of-right
                           (update-data
                             (make-tree
                               node
                               left
                               left-of-right))
                           right-of-right)))
                      (else
                        (update-data tree))))))))
  (define (insert key value tree)
    (if (null? tree)
      (make-tree
        (make-node key
                   priority
                   data
                   value)
        nil
        nil)
      (let ((node (tree-node tree))
            (left (tree-left tree))
            (right (tree-right tree)))
        (let ((k (node-key node)))
          (cond ((key-less? key k)
                 (fixup
                   (make-tree node
                              (insert key value left)
                              right)))
                ;; swallow duplicate insertions
                ((equal? key k)
                 tree)
                ;; flip a coin to decide to go left or right,
                ;; if only part of k and key equals?
                (else
                  (fixup
                    (make-tree node
                               left
                               (insert key value right)))))))))
  (insert key value tree))

(define (treap-delete key tree key-less? update-data)
  (define (delete-root tree)
    (let ((node (tree-node tree))
          (left (tree-left tree))
          (right (tree-right tree)))
      (cond ((and (null? left) (null? right))
             nil)
            ((null? left)
             right)
            ((null? right)
             left)
            (else
              (let ((left-of-right (tree-left right))
                    (right-of-right (tree-right right))
                    (node-of-right (tree-node right))
                    (left-of-left (tree-left left))
                    (right-of-left (tree-right left))
                    (node-of-left (tree-node left)))
                (if (priority-less? left right)
                  (update-data
                    (make-tree
                      node-of-right
                      (delete-root
                        (make-tree
                          node
                          left
                          left-of-right))
                      right-of-right))
                  (update-data
                    (make-tree
                      node-of-left
                      left-of-left
                      (delete-root
                        (make-tree
                          node
                          right-of-left
                          right))))))))))
  (define (delete key tree)
    (if (null? tree)
      tree
      (let ((node (tree-node tree))
            (left (tree-left tree))
            (right (tree-right tree)))
        (let ((k (node-key node)))
          (cond ((key-less? key k)
                 (make-tree node
                            (delete key left)
                            right))
                ((equal? key k)
                 (delete-root tree))
                (else
                  (make-tree node
                             left
                             (delete key right))))))))
  (delete key tree))

(use-modules (ice-9 pretty-print))

(define tree-1 (interval-insert
                 '(2 . 5)
                 19
                 (interval-insert
                   '(4 . 7)
                   83
                   (interval-insert
                     '(6 . 9)
                     1
                     (interval-insert
                       '(8 . 11)
                       27
                       (interval-insert
                         '(10 . 13)
                         8
                         (interval-insert
                           '(12 . 15)
                           2
                           '())))))))

(define tree-2 (interval-insert
                 '(4 . 5)
                 19
                 (interval-insert
                   '(4 . 7)
                   82
                   (interval-insert
                     '(4 . 9)
                     85
                     (interval-insert
                       '(4 . 11)
                       20
                       (interval-insert
                         '(4 . 13)
                         8
                         (interval-insert
                           '(4 . 15)
                           4
                           '())))))))

(define (give-me-a-hash-tree size)
  (define hash-space (expt 2 15))
  (let loop ((i 0)
             (tree nil))
    (if (> i size)
      tree
      (loop (+ i 1)
            (hash-insert (random hash-space) tree)))))

(define tree-3 (give-me-a-hash-tree (expt 2 4)))

(pretty-print tree-1)
(pretty-print tree-2)
(pretty-print (interval-delete '(8 . 11) tree-1))
(pretty-print (interval-delete '(4 . 9) tree-2))
(pretty-print (interval-search '(2 . 3) tree-1))
(pretty-print (interval-search '(16 . 19) tree-1))
(pretty-print (interval-search '(0 . 1) tree-1))
(pretty-print tree-3)
(pretty-print (hash-search (random 1983) tree-3))
