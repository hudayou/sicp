(define false #f)

(define true #t)

;; set as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (define (union s1 s2)
    (cond ((null? s1) s2)
          ((null? s2) s1)
          ((element-of-set? (car s1) s2)
           (union (cdr s1) s2))
          (else (union (cdr s1) (cons (car s1) s2)))))
  (union (reverse set1) set2))

;; allow duplicate elements in list representation
;; eg: {1,2,3} could be represented as (2 3 2 1 3 2 2).

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))

;; set as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (define (adjoin x less more)
    (if (null? more)
      (reverse (cons x less))
      (let ((carm (car more))
            (cdrm (cdr more)))
        (cond ((> x carm) (adjoin x (cons carm less) cdrm))
              ((= x carm)
               (append (reverse (cons x less)) cdrm))
              ((> x carm)
               (append (reverse (cons x less)) more))))))
  (adjoin x '() set))

;; '(1 3 5) '(4 8 9)
(define (union-set set1 set2)
  ;; s3 is a set whoes elements are less than
  ;; any elements in s1 and s2
  ;; elements in s3 are in order opposite to elements
  ;; in s1 and s2
  (define (union s1 s2 s3)
    (cond ((null? s1) (append (reverse s3) s2))
          ((null? s2) (append (reverse s3) s1))
          (else
            (let ((car1 (car s1))
                  (cdr1 (cdr s1))
                  (car2 (car s2))
                  (cdr2 (cdr s2)))
              (cond ((< car1 car2)
                     (union cdr1 s2 (cons car1 s3)))
                    ((= car1 car2)
                     (union cdr1 cdr2 (cons car1 s3)))
                    ((> car1 car2)
                     (union s1 cdr2 (cons car2 s3))))))))
  (union set1 set2 '()))

;; set as binary trees
;; binary trees are represented as deep lists

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (union-set set1 set2)
  (define (union s1 s2 s3)
    (cond ((null? s1) (append (reverse s3) s2))
          ((null? s2) (append (reverse s3) s1))
          (else
            (let ((car1 (car s1))
                  (cdr1 (cdr s1))
                  (car2 (car s2))
                  (cdr2 (cdr s2)))
              (cond ((< car1 car2)
                     (union cdr1 s2 (cons car1 s3)))
                    ((= car1 car2)
                     (union cdr1 cdr2 (cons car1 s3)))
                    ((> car1 car2)
                     (union s1 cdr2 (cons car2 s3))))))))
  (let ((s1 (tree->list-2 set1))
        (s2 (tree->list-2 set2)))
    (let ((s3 (union s1 s2 '())))
      (list->tree s3))))

(define (intersection-set set1 set2)
  (define (intersect set1 set2)
    (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersect (cdr set1)
                                (cdr set2))))
              ((< x1 x2)
               (intersect (cdr set1) set2))
              ((< x2 x1)
               (intersect set1 (cdr set2)))))))
  (let ((s1 (tree->list-2 set1))
        (s2 (tree->list-2 set2)))
    (let ((s3 (intersect s1 s2)))
      (list->tree s3))))

;; T(tree) = T(left-tree) + T(right-tree)
;; recursive process
;;
;; (trace tree->list-1 tree-1)
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

;; T(tree, list) = T(left-tree, T(right-tree, list))
;; iterative process, see knuth's method for using
;; a stack to assist tree travesal
;; algorithm:
;; traverse the right tree untill we meet a subtree whose left
;; sub tree is null, then traverse the left tree.
;;
;; (trace tree->list-2 tree-1)
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define tree-1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define tree-2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree
                          9
                          '()
                          (make-tree 11 '() '())))))

(define tree-3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))

(define tree-4
  (make-tree 6
             (make-tree 5
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 8 '() '())
                        (make-tree 11 '() '()))))

(define tree-5
  (make-tree 4
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 8
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; create a binary tree with n elements from list elts
;; size of elts must be greater than or equal to n
;;
;; base case:
;; n = 0, (() elts) is returned
;; otherwise:
;; take floor of (n-1)/2 as left-size elements from elts to be left tree,
;; take the first element of the remaining list as root,
;; take n - left-size - 1 elements from the elements left in elts
;; to be the right tree.
;; make a tree from root ,left tree and right tree
;; then cons the tree with the other elements in elts
;;
;; (list->tree '(1 3 5 7 9 11)) looks like below:
;;                     5
;;                 1       9
;;                    3 7     11
;;
;; T(n) = T(floor of (n-1)/2) + T(n - 1 - floor of (n-1)/2) + O(1)
;; let's be sloppy here, since O(1) is polynominal smaller than O(n)
;; aka O(n^(log2 of 2))
;; we can apply master theorem case 1:
;; T(n) = 2T(n/2) + O(1)
;;      = O(n)
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))
