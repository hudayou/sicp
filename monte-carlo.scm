(define (time-in-ms)
  (let ((time (gettimeofday)))
    (let ((second (car time))
          (microsecond (cdr time)))
      (+ (* second 1000000) microsecond))))
(define (rand)
  (random (time-in-ms)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral trials p x1 x2 y1 y2)
  (* (abs (- x1 x2))
     (abs (- y1 y2))
     (monte-carlo trials p)))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (square x)
  (* x x))
(define (estimate-circle trials center-x center-y radius)
  (let ((x1 (+ center-x radius))
        (x2 (- center-x radius))
        (y1 (+ center-y radius))
        (y2 (- center-y radius)))
    (define (in-circle-test)
      (let ((test-x (random-in-range x2 x1))
            (test-y (random-in-range y2 y1)))
        (<= (+
              (square (- test-x center-x))
              (square (- test-y center-y)))
            (square radius))))
    (estimate-integral trials in-circle-test x1 x2 y1 y2)))
