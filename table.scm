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
    ;; lookup car of keys in table,
    ;; if cdr of keys is #nil and a record is found,
    ;; set the cdr of the record to value;
    ;; if cdr of keys is #nil and a record is not found,
    ;; set the table to be new backboned key value record and the cdr
    ;; of the table;
    ;; if cdr of keys is not #nil and a record is found,
    ;; insert the cdr of keys to record;
    ;; if cdr of keys is not #nil and a record is not found,
    ;; insert the cdr of keys and value to newly created table
    ;; (cons (car keys) '())
    ;; and attach it with cdr of the table.
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
(define (make-table less-key?)
  ;; internal table implemented as binary tree
  (let ((local-table '()))
    (define (empty?)
      (null? local-table))
    (define (lookup key)
      (lookup-in-tree key local-table))
    (define (lookup-in-tree key tree)
      (cond ((null? tree) #f)
            ((equal? key (tree-key tree))
             (tree-value tree))
            ((less-key? key (tree-key tree))
             (lookup-in-tree key (tree-left tree)))
            (else
              (lookup-in-tree key (tree-right tree)))))
    (define (insert! key value)
      (if (null? local-table)
        (set! local-table (make-tree key value))
        (insert-in-tree! key value local-table))
      'ok)
    (define (insert-in-tree! key value tree)
      (cond ((equal? key (tree-key tree))
             (set-tree-value! tree value))
            ((less-key? key (tree-key tree))
             (if (null? (tree-left tree))
               (set-tree-left! tree (make-tree key value))
               (insert-in-tree! key value (tree-left tree))))
            (else
             (if (null? (tree-right tree))
               (set-tree-right! tree (make-tree key value))
               (insert-in-tree! key value (tree-right tree))))))
    (define (make-tree key value)
      (cons (cons key '())
            (cons value '())))
    (define (tree-left tree)
      (cdar tree))
    (define (tree-right tree)
      (cddr tree))
    (define (tree-key tree)
      (caar tree))
    (define (tree-value tree)
      (cadr tree))
    (define (set-tree-value! tree value)
      (set-car! (cdr tree) value))
    (define (set-tree-left! tree left)
      (set-cdr! (car tree) left))
    (define (set-tree-right! tree right)
      (set-cdr! (cdr tree) right))
    (define (table)
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'table) table)
            (else (error "unknown operation -- table" m))))
    dispatch))
(define binary-tree-table (make-table <))
(define get (binary-tree-table 'lookup-proc))
(define put (binary-tree-table 'insert-proc!))
(define table (binary-tree-table 'table))
