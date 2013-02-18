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
(define f #f)
(set!
  f
  (let ((state 0)
        (locked #f))
    (lambda (a)
      (cond ((zero? a) (set! locked #t)
                       (set! state a)
                       state)
            (locked state)
            (else a)))))
(display (+ (f 0) (f 1)))
(newline)