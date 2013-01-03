(define-module (sicp utils))

(export sum)

(define (sum lst)
  ;; sum a list from last element to first element
  ;; return the sum list
  ;; (1 2 3) will be (6 5 3)
  (define (sum-lst lst res)
    ;; add the first element of two lists
    ;; lst can't be '()
    ;; if res is '() return (car lst)
    ;; otherwise return the sume of them
    (define (add lst res)
      (if (null? res)
        (car lst)
        (+ (car lst) (car res))))
    (if (null? lst)
      res
      (sum-lst
        (cdr lst)
        (append (list (add lst res)) res))))
  (sum-lst (reverse lst) '()))
