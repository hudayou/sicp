(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "front called with an empty queue" queue)
    (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "delete! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

;; a queue with null front pointer and
;; null rear pointer
(define q1 (make-queue))
;; a queue with front pointer
;; and rear pointer both pointed to '(a)
(insert-queue! q1 'a)
;; a queue with front pointer pointed to '(a)
;; and rear pointer pointed to '(b)
(insert-queue! q1 'b)
;; a queue with front pointer pointed to '(b)
;; and rear pointer both pointed to '(b)
(delete-queue! q1)
;; a queue with null front pointer
;; and rear pointer pointed to '(b)
(delete-queue! q1)

(define (print-queue queue)
  (define (queue->list queue)
    (front-ptr queue))
  (display (queue->list queue))
  (newline))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (front)
      front-ptr)
    (define (rear)
      rear-ptr)
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "front called with an empty queue")
        (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
                (set-cdr! (rear) new-pair)
                (set-rear-ptr! new-pair)
                dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "delete! called with an empty queue"))
            (else
              (set-front-ptr! (cdr (front)))
              dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front)
            ((eq? m 'rear-ptr) rear)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else
              (error "undefined queue op" m))))
    dispatch))

(define (front-ptr queue)
  ((queue 'front-ptr)))

(define (rear-ptr queue)
  ((queue 'rear-ptr)))

(define (set-front-ptr! queue item)
  ((queue 'set-front-ptr!) item))

(define (set-rear-ptr! queue item)
  ((queue 'set-rear-ptr!) item))

(define (empty-queue? queue)
  ((queue 'empty-queue?)))

(define (front-queue queue)
  ((queue 'front-queue)))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (front-ptr deque)
  (car deque))
(define (rear-ptr deque)
  (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (make-deque)
  (cons '() '()))
(define (empty-deque? deque)
  (or (null? (front-ptr deque))
      (null? (rear-ptr deque))))
(define (front-deque deque)
  (if (empty-deque? deque)
    (error "front called with an empty deque" deque)
    (item (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "rear called with an empty deque" deque)
    (item (rear-ptr deque))))
(define (make-triple item)
  (cons
    (cons item '())
    '()))
(define (prev triple)
  (cdr triple))
(define (next triple)
  (cdar triple))
(define (data triple)
  (car triple))
(define (item triple)
  (caar triple))
(define (set-prev! triple prev)
  (set-cdr! triple prev))
(define (set-next! triple next)
  (set-cdr! (car triple) (car next)))
(define (front-insert-deque! deque item)
  (let ((new-triple (make-triple item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-triple)
           (set-rear-ptr! deque new-triple)
           deque)
          (else
            (set-next! new-triple (front-ptr deque))
            (set-prev! (front-ptr deque) new-triple)
            (set-front-ptr! deque new-triple)
            deque))))
(define (rear-insert-deque! deque item)
  (let ((new-triple (make-triple item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-triple)
           (set-rear-ptr! deque new-triple)
           deque)
          (else
            (set-prev! new-triple (rear-ptr deque))
            (set-next! (rear-ptr deque) new-triple)
            (set-rear-ptr! deque new-triple)
            deque))))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "delete! called with an empty deque" deque))
        (else
          (if (null? (next (front-ptr deque)))
            (set-front-ptr! deque (next (front-ptr deque)))
            (begin
              (set-prev! (next (front-ptr deque)) '())
              (set-front-ptr! deque (next (front-ptr deque)))))
          deque)))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "delete! called with an empty deque" deque))
        (else
          (if (null? (prev (rear-ptr deque)))
            (set-rear-ptr! deque (prev (rear-ptr deque)))
            (begin
              (set-next! (prev (rear-ptr deque)) '())
              (set-rear-ptr! deque (prev (rear-ptr deque)))))
          deque)))
(define (print-deque deque)
    (cond ((empty-deque? deque)
           '())
          (else
            (let loop ((triple (rear-ptr deque))
                       (lst '()))
              (if (null? triple)
                lst
                (loop (prev triple) (cons (item triple) lst)))))))
(define (print-deque deque)
  (if (empty-deque? deque)
    '()
    (data (front-ptr deque))))