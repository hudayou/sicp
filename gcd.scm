(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;; (gcd 206 40)
;; Applicative Order 4 times remainder operation
;; (gcd 206 40) (remainder 206 40) -> (gcd 40 6)
;; (remainder 40 6) -> (gcd 6 4)
;; (remainder 6 4) -> (gcd 4 2)
;; (reaminder 4 2) -> (gcd 2 0)
;; -> 2
;;
;; Normal Order 18 times remainder operation
;; (gcd 206 40) (remainder 206 40) -> (gcd 40 (remainder 206 40))
;; (= (remainder 206 40) 0) -> 
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (= (remainder 40 (remainder 206 40)) 0) ->
;; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40)
;; (remainder 40 (remainder 206 40)))) ->
;; (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ->
;; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder
;;      206 40) (remainder 40 (remainder 206 40))))) ->
;; (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206
;; 40) (remainder 40 (remainder 206 40)))) 0) ->
;; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;
