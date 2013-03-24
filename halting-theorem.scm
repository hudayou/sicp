(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p)
    (run-forever)
    'halted))
(try try)

;; if it's possible to write a procedure halts? that correctly determines
;; whether p halts on a for any procedure p and object a.
;; 
;; if try halt on try, then (try try) returns a value,
;; but the programs show us that it will run-forever.
;;
;; if try does not halt on try, then (try try) does not return a value,
;; but the program show us that it will return 'halted.
;;
;; in both cases, we get a contradiction. so it is not possible to write such
;; a procedure.
;; 
;; this explains why we can't stop a program in a turning machines to run
;; forever.
