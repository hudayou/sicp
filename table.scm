(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable
                    (cons (cons key-2 value)
                          (cdr subtable)))))
      (set-cdr! table
                (cons (list key-1
                            (cons key-2 value))
                      (cdr table)))))
  'ok)
;; (define operation-table (make-table))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((not (pair? records)) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key . extra-keys)
      (lookup-keys (cons key extra-keys) local-table))
    ;; lookup car of keys in local-table, if a record is found,
    ;; and if cdr of keys is not #nil, lookup cdr of keys in cdr of the record.
    ;; if cdr of key is #nil, return cdr of the record.
    ;; if no record is found return #f.
    (define (lookup-keys keys table)
      (let ((record (assoc (car keys) (cdr table))))
        (if record
          (if (null? (cdr keys))
            (cdr record)
            (lookup-keys (cdr keys) record))
          #f)))
    (define (insert! value key . extra-keys)
      (insert-keys! (cons key extra-keys) value local-table)
      'ok)
    (define (insert-keys! keys value table)
      (let ((record (assoc (car keys) (cdr table))))
        (if (null? (cdr keys))
          (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons (car keys) value) (cdr table))))
          (if record
            (insert-keys! (cdr keys) value record)
            (set-cdr! table
                      (cons (insert-keys! (cdr keys)
                                          value
                                          (cons (car keys) '()))
                            (cdr table))))))
      table)
    (define (table)
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'table) table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define tolerance-table (make-table (lambda (a b)
                                      (if (and (number? a)
                                               (number? b))
                                        (< a (+ b 0.1))
                                        (equal? a b)))))
(define (same-key? a b)
  (if (and (number? a)
           (number? b))
    (< a (+ b 0.1))
    (equal? a b)))
(define get (tolerance-table 'lookup-proc))
(define put (tolerance-table 'insert-proc!))
(define table (tolerance-table 'table))
