(use-modules (sicp utils))

(define debug #f)

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (let ((point (cons x y)))
    (if debug
      (print-point point)
      "")
    point))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (distance p1 p2)
  (let ((dx (- (x-point p1) (x-point p2)))
        (dy (- (y-point p1) (y-point p2))))
    (sqrt (+ (square dx) (square dy)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(midpoint-segment (make-segment (make-point 1 1) (make-point 3 3)))

;;(define (make-rectangle bottom-left bottom-right top-right top-left)
;;  (cons (cons bottom-left bottom-right) (cons top-left top-right)))
;;(define (bottom-left rectangle)
;;  (car (car rectangle)))
;;(define (bottom-right rectangle)
;;  (cdr (car rectangle)))
;;(define (top-left rectangle)
;;  (car (cdr rectangle)))
;;(define (top-right rectangle)
;;  (cdr (cdr rectangle)))
;;(define (height rectangle)
;;  (distance (bottom-left rectangle) (top-left rectangle)))
;;(define (width rectangle)
;;  (distance (bottom-left rectangle) (bottom-right rectangle)))
;;
(define (make-rectangle bottom-left top-right)
  (cons bottom-left top-right))
(define (bottom-left rectangle)
  (car rectangle))
(define (top-right rectangle)
  (cdr rectangle))
(define (bottom-right rectangle)
  (make-point (x-point (top-right rectangle))
              (y-point (bottom-left rectangle))))
(define (top-left rectangle)
  (make-point (x-point (bottom-left rectangle))
              (y-point (top-right rectangle))))
(define (height rectangle)
  (distance (bottom-left rectangle) (top-left rectangle)))
(define (width rectangle)
  (distance (bottom-left rectangle) (bottom-right rectangle)))
              

;; use the same conventional interface for different representations
(define (perimeter-rectangle rectangle)
  (let ((h (height rectangle))
        (w (width rectangle)))
    (+ h h w w)))

;; use the same conventional interface for different representations
(define (area-rectangle rectangle)
  (let ((h (height rectangle))
        (w (width rectangle)))
    (* h w)))

;;(perimeter-rectangle (make-rectangle
;;                       (make-point 0 0)
;;                       (make-point 1 0)
;;                       (make-point 1 1)
;;                       (make-point 0 1)))
;;
;;(area-rectangle (make-rectangle
;;                  (make-point 0 0)
;;                  (make-point 1 0)
;;                  (make-point 1 1)
;;                  (make-point 0 1)))

(perimeter-rectangle (make-rectangle
                       (make-point 0 0)
                       (make-point 1 1)))

(area-rectangle (make-rectangle
                  (make-point 0 0)
                  (make-point 1 1)))
