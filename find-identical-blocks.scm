;; Find identical code blocks in a file(directory).

(define nil '())

;; block-size-range is a range specify the size of the code block:
;; (min-block-size max-block-size)
;; A file with n lines have
;; (max-block-size - min-block-size + 1) * n -
;; (fold + 0 (enumerate min-block-size - 1 max-block-size - 1))
;; blocks.
;; (define block-size-range (list 4 12))
(define min-block-size 5)
(define max-block-size 67)
;;(define min-block-size 3)
;;(define max-block-size 43)
;;(define min-block-size 7)
;;(define max-block-size 47)
;;(define min-block-size 5)
;;(define max-block-size 47)
;;(define min-block-size 7)
;;(define max-block-size 67)

;; The minimum times of a block is repated in the file.
(define min-repeat-factor 2)

;; The maximum times of a block is repated in the file.
(define max-repeat-factor 6)

;; How a code block record looks like?
;; A vector looks like below:
;; #(hash-of-block path-to-file start-line end-line continous-flag)

(define (make-block start-line end-line path-to-file list-of-lines)
  (vector
    (string-hash (string-join list-of-lines delimiter-string))
    path-to-file
    start-line
    end-line
    (if (member "" list-of-lines) #f #t)))

(define (block-hash block)
  (vector-ref block 0))

(define (block-path block)
  (vector-ref block 1))

(define (block-start block)
  (vector-ref block 2))

(define (block-end block)
  (vector-ref block 3))

(define (block-size block)
  (if (pair? block)
    (block-size (car block))
    (+ 1 (- (block-end block) (block-start block)))))

(define (block-content block)
  (let ((bsize (block-size block))
        (bpath (block-path block))
        (bstart (block-start block)))
    (string-join
      (list-head
        (list-tail (read-lines bpath) (- bstart 1))
        bsize)
      delimiter-string)))

(define (block? block)
  (vector? block))

(define (block-continous? block)
  (vector-ref block 4))

(define (block-overlaps? block1 block2)
  (or
    (and (>= (block-start block1) (block-start block2))
         (<= (block-end block1) (block-end block2))
         (string=? (block-path block1) (block-path block2)))
    (and (<= (block-start block1) (block-start block2))
         (>= (block-end block1) (block-end block2))
         (string=? (block-path block1) (block-path block2)))))

(define (block-hash=? block1 block2)
  (= (block-hash block1)
     (block-hash block2)))

;; A block list is a list of blocks with same content

(define (block-list-overlaps? lst1 lst2)
  (if (and (null? lst1) (null? lst2))
    #t
    (and (= (length lst1) (length lst2))
         (block-overlaps? (car lst1) (car lst2))
         (block-list-overlaps? (cdr lst1) (cdr lst2)))))

;; The algorithm:
;; Build blocks from the file, insert list of identical blocks
;; in a hash table keyed by the string hash of the content of the block.
;; (Note blocks which contains blank lines are not inserted.)
;;
;; Remove single blocks, which means they have no companies in the file.
;;
;; Remove list of blocks which overlaps with other list of blocks and
;; have a smaller block size.

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(use-modules (ice-9 rdelim))

(define (read-file port)
  (read-delimited "" port))

(define delimiter #\newline)
(define delimiter-string "\n")

(define (read-lines file)
  (string-split
    (string-trim-right
      (call-with-input-file file read-file)
      delimiter)
    delimiter))

;; A fold operation, use fold instead?
(define (and-filters filters param)
  (if (null? filters)
    #t
    (and ((car filters) param)
         (and-filters (cdr filters) param))))

;; start-line may start with '-
(define start-line-regexp "^\\s+[-a-z]+:\\s*[|>]*\\s*$")
;; end-line may not start with '-
(define end-line-regexp "^\\s+[.a-z].*$")

;; filter start-line and end-line by regular expression
(define (se-filter list-of-lines)
  (let ((start-line (list-ref list-of-lines 0))
        (end-line (list-ref list-of-lines (- (length list-of-lines) 1))))
    (if (and (string-match start-line-regexp start-line)
             (string-match end-line-regexp end-line))
      #t
      #f)))

(define block-filters (list se-filter))

(define (build-block-hash-table file)
  (define block-hash-table (make-hash-table))
  (define list-of-lines (read-lines file))
  (define (remove-single-blocks)
    (for-each
      (lambda (x)
        (if (< (length (cdr x)) min-repeat-factor)
          (hash-remove! block-hash-table (car x))))
      (hash-map->list cons block-hash-table))
    block-hash-table)
  (define (build-hash-table line-list start-line)
    (if (< (length line-list) min-block-size)
      block-hash-table
      (begin
        (update-hash-table line-list start-line)
        (build-hash-table (cdr line-list) (+ start-line 1)))))
  (define (update-hash-table line-list start-line)
    (for-each
      (lambda (b)
        (let ((b-key (block-hash b)))
          (let ((b-value (hash-ref block-hash-table b-key)))
            (if (block-continous? b)
              (if b-value
                (hash-set! block-hash-table b-key (cons b b-value))
                (hash-set! block-hash-table b-key (list b)))))))
      (build-blocks line-list start-line)))
  (define (filter-block-content list-of-lines)
    (and-filters block-filters list-of-lines))
  (define (build-blocks line-list start-line)
    (filter block?
            (map
              (lambda (bsize)
                (let ((list-of-lines (list-head line-list bsize)))
                  (if (filter-block-content list-of-lines)
                    (make-block
                      start-line
                      (+ start-line bsize -1)
                      file
                      list-of-lines))))
              (enumerate-interval min-block-size
                                  (let ((no-of-lines (length line-list)))
                                    (if (< no-of-lines max-block-size)
                                      no-of-lines
                                      max-block-size))))))
  (build-hash-table list-of-lines 1)
  (remove-single-blocks))

(define (hash-empty? hash-table)
  (eq? (hash-map->list cons hash) nil))

(define (remove-duplicate-entries hash-table)
  (define (remove-overlaps k1 v1)
    (for-each
      (lambda (x)
        (let ((k2 (car x))
              (v2 (cdr x)))
          (let ((s2 (block-size v2))
                (s1 (block-size v1)))
            (if (block-list-overlaps? v1 v2)
              (if (< s2 s1)
                (hash-remove! hash-table k2))))))
      (hash-map->list cons hash-table)))
  ;; prove by contradiction
  ;; since for each block list, block lists overlaps
  ;; with it and have a smaller block size is removed.
  ;;
  ;; if there is such a block that overlaps with other,
  ;; two cases:
  ;; the other block have a smaller size, then it should be removed already.
  ;; otherwise, the block itself should already be removed.
  (for-each
    (lambda (x)
      (remove-overlaps (car x) (cdr x)))
    (hash-map->list cons hash-table))
  hash-table)

(define (hash-show hash-table)
  (hash-for-each
    (lambda (k v)
      (if (> (length v) 1)
        (begin
          (display k)
          (display "\t***\t")
          (if (> (length v) 2)
            (begin
              (display (list-head v 2))
              (newline)
              (display (- (length v) 2))
              (display " more .."))
            (display v))
          (newline)
          (display (block-content (car v)))
          (newline))))
    hash-table))

(define (find-identical-blocks file)
  (hash-show
    (remove-duplicate-entries
      (build-block-hash-table file))))

(define (wrapper-of-fib file stat flag)
  (if (eq? flag `regular)
    (find-identical-blocks file))
  #t)

(use-modules (ice-9 ftw))

(define (fib file)
  (ftw file wrapper-of-fib))
