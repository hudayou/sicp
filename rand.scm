(define (time-in-ms)
  (let ((time (gettimeofday)))
    (let ((second (car time))
          (microsecond (cdr time)))
      (+ (* second 1000000) microsecond))))
(define rand #f)
(let ((seed 0)
      (new #t))
  (set! rand
    (lambda (op)
      (define (generate)
        (if new
          (begin
            (set! *random-state* (seed->random-state seed))
            (set! new #f)))
        (random (time-in-ms)))
      (define (reset new-seed)
        (set! seed new-seed)
        (set! new #t)
        seed)
      (cond ((eq? op 'generate) (generate))
            ((eq? op 'reset) reset)))))
