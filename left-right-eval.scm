(define (make-locker state locked)
  (lambda (a)
    (cond ((zero? a) (set! locked #t)
                     (set! state a)
                     state)
          (locked (set! locked #f)
                  state)
          (else a))))
(define locker (make-locker 0 #f))
(define (f a)
  (locker a))
(display (+ (f 0) (f 1)))
(newline)
