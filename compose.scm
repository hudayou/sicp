(use-modules (sicp utils))

(define inc 1+)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(display (= ((compose square inc) 6) 49))
(newline)
