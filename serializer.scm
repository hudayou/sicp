(define true #t)
(define false #f)

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
    true
    (begin (set-car! cell true)
           false)))

;; if test-and-set! is not atomic then if when a and b access the cell
;; and cell is false, then they could both complete test-and-set!
;; and escape the the-mutex loop.

(define (make-semaphore n)
  (let ((count 0)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (< count n)
               (begin
                 (set! count (+ count 1))
                 (mutex 'release))
               (begin
                 (mutex 'release)
                 (the-semaphore m))))
            ((eq? m 'release)
             (mutex 'acquire)
             (if (> count 0)
               (set! count (- count 1)))
             (mutex 'release))))
    the-semaphore))
