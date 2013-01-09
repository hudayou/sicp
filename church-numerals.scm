;; functions returned take one nonnegative integers
;; as argument

;; put in something into zero
;; it returns a function takes one
;; argument which returns the argument.
;; (zero g) -> (f x)
(define zero (lambda (f) (lambda (x) x)))

;; (add-1 g) -> (lambda (f) (lambda (x) (f ((g f) x))))
;; let g be zero,
;; then it will return (f x)
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;;Refers to:
;;http://en.wikipedia.org/wiki/Church_encoding#Computation_with_Church_numerals
(define (add m n)
  (lambda (f)
    (lambda (x) ((m f) ((n f) x)))))

(((add zero two) 1+) 4)

(((add-1 two) 1+) 4)

((one 1+) 4)

((zero 1+) 4)

((two 1+) 4)
