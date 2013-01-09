(define (make-rat n d)
  (let ((g (gcd n d))
        (s (positive? (* n d))))
    (if s
      (cons (abs (/ n g)) (abs (/ d g)))
      (cons (- (abs (/ n g))) (abs (/ d g))))))
