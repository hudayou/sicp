(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
            (error "unknown op -- make-from-real-imag" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else
            (error "unknow op -- make-from-mag-ang" op))))
  dispatch)

;; new type is added
;;
;; explicit dispatch:
;; new operation names, change the dispatch logic.
;;
;; data directed style:
;; install new slot in the type-operation table.
;;
;; message-passing style:
;; add a new intelligent data object.
;;
;; if new types must often be added, message passing is more approriate.
;; if new operations must often be added, data directed style is more
;; approriate.
