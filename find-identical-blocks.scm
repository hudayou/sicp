;; Find identical code blocks in a file(directory).

(define nil '())

;; block-size-range is a range specify the size of the code block:
;; (min-block-size max-block-size)
;; A file with n lines have
;; (max-block-size - min-block-size + 1) * n -
;; (map + (enumerate min-block-size - 1 max-block-size - 1))
;; blocks.
;; (define block-size-range (list 4 12))
(define min-block-size 4)
(define max-block-size 18)

;; The minimum times of a block is repated in the file.
(define min-repeat-factor 2)

;; How a code block record looks like?
;; A list looks like below:
;; (hash-of-block path-to-file start-line end-line)

(define (make-block start-line end-line path-to-file content-of-block)
  (list (string-hash content-of-block) path-to-file start-line end-line))

(define (block-hash block)
  (list-ref block 0))

(define (block-path block)
  (list-ref block 1))

(define (block-start block)
  (list-ref block 2))

(define (block-end block)
  (list-ref block 3))

(define (block-size block)
  (- (block-end block) (block-start block)))

(define (block-overlaps? block1 block2)
  (if (>= (block-start block1) (block-start block2))
    (and (>= (block-start block1) (block-start block2))
         (<= (block-end block1) (block-end block2))
         (string=? (block-path block1) (block-path block2)))
    (and (>= (block-start block2) (block-start block1))
         (<= (block-end block2) (block-end block1))
         (string=? (block-path block1) (block-path block2)))))

(define (block-hash=? block1 block2)
  (= (block-hash block1)
     (block-hash block2)))

;; The algorithm:
;; Scan the file, get a list of code blocks.
;; Find blocks which have the same hash and group them into
;; a list(greedy algorithm can be used here).
;; Filter the list of lists,
;; keep the lists whose number of elements should be at least
;; min-repeate-factor. 
;; Filter the list of lists,
;; eliminate list of blocks overlaps other list of blocks.
;; That is the two list of block have the same length and
;; each block of the first list overlaps the second list.
;; The list of blocks must be sorted by start-line.

(define (group proc sequence)
  (define (any res carl block)
    (if (null? carl)
      res
      (any (or res (proc (car carl) block))
           (cdr carl)
           block)))
  (define (insert block lst1 lst2 attach)
    (if (null? lst1)
      (if attach
        (append lst2 (list (list block)))
        lst2)
      (let ((carl (car lst1)))
        (cond ((any #f carl block)
               (insert block (cdr lst1)
                       (append lst2 (list (append carl (list block))))
                       #f))
              (else
                (insert block (cdr lst1) (append lst2 (list carl)) attach))))))
  (define (iter block-list list-of-block-list)
    (if (null? block-list)
      list-of-block-list
      (iter (cdr block-list)
            (insert (car block-list)
                    list-of-block-list
                    nil
                    #t))))
  (iter sequence nil))

(define (group-block list-of-block)
  (group block-hash=? list-of-block))

(define (filter-block list-of-block)
  (>= (length list-of-block) min-repeat-factor))

(define (block-list-overlaps? block-list1 block-list2)
  (define (less lst1 lst2)
    (<= (block-start lst1) (block-start lst2)))
  (define (iter lst1 lst2 res)
    (if (null? lst1)
      res
      (if (block-overlaps? (car lst1) (car lst2))
        (iter (cdr lst1) (cdr lst2) (and res #t))
        (iter (cdr lst1) (cdr lst2) (and res #f)))))
  (if (not (= (length block-list1)
              (length block-list2)))
    #f
    (if (< (block-size (car block-list1)) (block-size (car block-list2)))
      (iter (sort block-list1 less) (sort block-list2 less) #t)
      (iter (sort block-list2 less) (sort block-list1 less) #t))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (group-block-list list-of-block-list)
  (group block-list-overlaps? list-of-block-list))

(use-modules (ice-9 rdelim))

(define (read-file port)
  (read-delimited "" port))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (build-block-list file)
  (define (sublist start result size list-of-lines)
    (if (= start size)
      result
      (sublist (+ start 1)
               (append result (list (car list-of-lines)))
               size
               (cdr list-of-lines))))
  (define (build-block-with-size lineno size list-of-lines)
    (if (> size (length list-of-lines))
      nil
      (make-block lineno
                  (+ lineno size -1)
                  file
                  (string-join
                    (sublist 0 nil size list-of-lines)))))
  (define (build-block lineno list-of-lines)
    (flatmap (lambda (size)
               (list (build-block-with-size lineno size list-of-lines)))
             (enumerate-interval min-block-size max-block-size)))
  (define (iter lineno list-of-lines list-of-block)
    (if (null? list-of-lines)
      list-of-block
      (iter (+ lineno 1)
            (cdr list-of-lines)
            (append list-of-block
                    (build-block lineno list-of-lines)))))
  (let ((file-content (call-with-input-file file read-file)))
    (iter 1 (string-split file-content #\newline) nil)))

(define (find-identical-blocks file)
  (define (less lst1 lst2)
    (and (<= (block-start (car lst1)) (block-start (car lst2)))
         (>= (block-end (car lst1)) (block-end (car lst2)))))
  (map (lambda (list-of-block-list)
         (car (sort list-of-block-list less)))
       (group-block-list
         (filter filter-block
                 (group-block
                   (filter (lambda (x)
                             (not (equal? x nil)))
                           (build-block-list file)))))))
