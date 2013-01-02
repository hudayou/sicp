;; tail call
(define (p) (q))
(define (q) (p))
;;(p)

;; tail call
(define (x) (y x))
(define (y x) (x))
;;(x)

;; linear recursive process
(define (m) (n (m)))
(define (n m) #t)
;;(x)
