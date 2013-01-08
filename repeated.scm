(use-modules (sicp utils))

(define (repeated f n)
  (lambda (x)
    (define (iter x j)
      (if (> j n)
        x
        (iter (f x) (+ j 1))))
    (iter x 1)))

(display ((repeated square 2) 5))
(newline)
