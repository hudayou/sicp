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
