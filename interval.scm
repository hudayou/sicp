;; interval represents resistor
(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

;; give 0 + sign
(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((< ux 0)
           ;; (-3 -2)
           (cond ((< uy 0)
                  ;; (-7 -5)
                  (make-interval (* ux uy) (* lx ly)))
                 ((> ly 0)
                  ;; (5 7)
                  (make-interval (* lx uy) (* ux ly)))
                 (else
                   ;; (-5 7)
                   (make-interval (* lx uy) (* lx ly)))))
          ((> lx 0)
           ;; (2 3)
           (cond ((< uy 0)
                  ;; (-7 -5)
                  (make-interval (* ux ly) (* lx uy)))
                 ((> ly 0)
                  ;; (5 7)
                  (make-interval (* lx ly) (* ux uy)))
                 (else
                   ;; (-5 7)
                   (make-interval (* ux ly) (* ux uy)))))
          (else
            ;; (-2 3)
            (cond ((< uy 0)
                   ;; (-7 -5)
                   (make-interval (* ux ly) (* lx ly)))
                  ((> ly 0)
                   ;; (5 7)
                   (make-interval (* lx uy) (* ux uy)))
                  (else
                    ;; (-5 7)
                    (let ((l (min (* lx uy) (* ux ly)))
                          (u (max (* lx ly) (* ux uy))))
                      (make-interval l u))))))))

(eq? (mul-interval (make-interval -3 -2) (make-interval -7 -5))
     (make-interval (* -2 -5) (* -3 -7)))
(eq? (mul-interval (make-interval -3 -2) (make-interval 5 7))
     (make-interval (* -3 7) (* -2 5)))
(eq? (mul-interval (make-interval -3 -2) (make-interval -5 7))
     (make-interval (* -3 7) (* -3 -5)))
(eq? (mul-interval (make-interval 2 3) (make-interval -7 -5))
     (make-interval (* 3 -7) (* 2 -5)))
(eq? (mul-interval (make-interval 2 3) (make-interval 5 7))
     (make-interval (* 2 5) (* 3 7)))
(eq? (mul-interval (make-interval 2 3) (make-interval -5 7))
     (make-interval (* 3 -5) (* 3 7)))
(eq? (mul-interval (make-interval -2 3) (make-interval -7 -5))
     (make-interval (* 3 -7) (* -2 -7)))
(eq? (mul-interval (make-interval -2 3) (make-interval 5 7))
     (make-interval (* -2 7) (* 3 7)))
(eq? (mul-interval (make-interval -2 3) (make-interval -5 7))
     (make-interval (min (* -2 7) (* 3 -5)) (max (* -2 -5) (* 3 7))))

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

