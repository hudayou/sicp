;; interval represents resistor
(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((or (= ly 0) (= uy 0))
           (error "divident spans zero " y))
          ((eq? (> ly 0) (> uy 0))
           (mul-interval x
                         (make-interval (/ 1.0 uy)
                                        (/ 1.0 ly))))
          (else
            (error "divident spans zero " y)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (minus-interval x)
  (make-interval (- (upper-bound x)) (- (lower-bound x))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; width of an interval is half of the difference between its upper and lower
;; bounds
;; (width (add-interval x y)) = (+ (width x) (width y))
;; (width x) = (width -x)
;; (width (sub-interval x y)) = (width (add-interval x -y))
;;                            = (+ (width x) (width -y))
;;                            = (+ (width x) (width y))
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

