(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below (beside smaller smaller) painter))))

;; high order procedures below

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (split b1 b2)
  (define (splitter painter n)
    (if (= n 0)
      painter
      (let ((smaller (splitter painter (- n 1))))
        (if (eq? b1 beside)
          (beside painter (below smaller smaller))
          (below (beside smaller smaller) painter)))))
  splitter)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (cons
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (cons
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (cons
    (* s (xcor-vect v))
    (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (cddr f))

(define a-frame (make-frame (make-vect 0 0) (make-vect 3 0) (make-vect 0 4)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

(equal? ((frame-coord-map a-frame) (make-vect 0 0))
        (origin-frame a-frame))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (outline-painter)
  (lambda (frame)
    (let ((bottom-left (make-vect 0 0))
          (bottom-right (make-vect 1 0))
          (top-left (make-vect 0 1))
          (top-right (make-vect 1 1)))
      ((segments->painter
         (list 
           (make-segment bottom-left bottom-right)
           (make-segment bottom-right top-right)
           (make-segment top-right top-left)
           (make-segment top-left bottom-left)))
       frame))))

(define (x-painter)
  (lambda (frame)
    (let ((bottom-left (make-vect 0 0))
          (bottom-right (make-vect 1 0))
          (top-left (make-vect 0 1))
          (top-right (make-vect 1 1)))
      ((segments->painter
         (list 
           (make-segment bottom-left top-right)
           (make-segment bottom-right top-left)))
       frame))))

(define (diamond-painter)
  (lambda (frame)
    (let ((left (make-vect 0 0.5))
          (right (make-vect 1 0.5))
          (top (make-vect 0.5 1))
          (bottom (make-vect 0.5 0)))
      ((segments->painter
         (list 
           (make-segment left top)
           (make-segment top right)
           (make-segment right bottom)
           (make-segment bottom left)
           ))
       frame))))

;; TODO: wave painter?
(define (wave-painter)
  (lambda (frame)
    (let ((left-eye-left    (make-vect 0.125 0.625))
          (left-eye-top     (make-vect 0.25  0.875))
          (left-eye-right   (make-vect 0.375 0.625))
          (right-eye-left   (make-vect 0.625 0.625))
          (right-eye-top    (make-vect 0.75  0.875))
          (right-eye-right  (make-vect 0.875 0.625))
          (mouth-left       (make-vect 0.25  0.5))
          (mouth-right      (make-vect 0.75  0.5))
          (mouth-bottom     (make-vect 0.5   0.25)))
      ((segments->painter
         (list 
           (make-segment left-eye-left left-eye-top)
           (make-segment left-eye-top left-eye-right)
           (make-segment right-eye-left right-eye-top)
           (make-segment right-eye-top right-eye-right)
           (make-segment mouth-left mount-bottom)
           (make-segment mouth-bottom mount-right)))
       frame))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)    ; new origin
                     (make-vect 1.0 1.0)    ; new end of edge1
                     (make-vect 0.0 0.0)))  ; new end of edge2

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)    ; new origin
                     (make-vect 0.0 0.0)    ; new end of edge1
                     (make-vect 1.0 1.0)))  ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0)))
          (paint-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below painter1 painter2)
  (rotate270
    (beside painter2 painter1)))

(define right-split (split beside below))
(define up-split (split below beside))

(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left up)
            (bottom-right right)
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))
