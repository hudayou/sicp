(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- choose-branch" bit))))
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
                             ; symbol
                             (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)(make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(use-modules (ice-9 pretty-print))
(pretty-print sample-tree)
(pretty-print sample-message)
(pretty-print (decode sample-message sample-tree))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode symbol tree bits)
    (if (leaf? tree)
      bits
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (if (member symbol (symbols left))
          (encode symbol left (cons 0 bits))
          (encode symbol right (cons 1 bits))))))
  (if (member symbol (symbols tree))
    (reverse (encode symbol tree '()))
    (error "bad symbol -- encode-symbol" symbol)))

(pretty-print
  (equal? (encode (decode sample-message sample-tree) sample-tree)
          sample-message))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
    (car leaf-set)
    (successive-merge
      (adjoin-set
        (make-code-tree
          (car leaf-set)
          (cadr leaf-set))
        (cddr leaf-set)))))

(define lyrics-tree (generate-huffman-tree
                      '((A 2) (NA 16) (BOOM 1) (SHA 3)
                              (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(pretty-print lyrics-tree)

(define sample-lyrics '(GET A JOB
                            SHA NA NA NA NA NA NA NA NA
                            GET A JOB
                            SHA NA NA NA NA NA NA NA NA
                            WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                            SHA BOOM))

(define sample-encoded-lyrics (encode sample-lyrics lyrics-tree))

(pretty-print sample-encoded-lyrics)
(display "length of sample-encoded-lyrics is ")
(pretty-print (length sample-encoded-lyrics))
(display "length of sample-lyrics is ")
(pretty-print (length sample-lyrics))
;; 108 bits will be needed
(pretty-print (* 100 (- 1 (/ 84.0 108))))

;; Suppose we have a Huffman tree for an alphabet of n symbols, and that the
;; relative frequencies of the symbols are 1, 2, 4, ..., 2^n-1.
;; 1 bit is needed to encode the most frequent symbol, n-1 bit is required to
;; encode the least frequent symbol.

;; Consider the encoding procedure that you designed in exercise 2.68. What is
;; the order of growth in the number of steps needed to encode a symbol? Be
;; sure to include the number of steps needed to search the symbol list at each
;; node encountered. To answer this question in general is difficult. Consider
;; the special case where the relative frequencies of the n symbols are as
;; described in exercise 2.71, and give the order of growth (as a function of
;; n) of the number of steps needed to encode the most frequent and least
;; frequent symbols in the alphabet.
;;
;; O(1) steps is needed for most frequent symbol
;; O(n^2) steps is needed for least frequent symbol
