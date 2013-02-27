;; Exercise 3.38. 
;; Suppose that Peter, Paul, and Mary share a joint bank account that initially
;; contains $100.
;; Concurrently, Peter deposits $10, Paul withdraws $20, and Mary withdraws
;; half the money in the account,
;; by executing the following commands:
;; Peter: (set! balance (+ balance 10))
;; Paul: (set! balance (- balance 20))
;; Mary: (set! balance (- balance (/ balance 2)))
;; a. List all the different possible values for balance after these three
;; transactions have been completed,
;; assuming that the banking system forces the three processes to run
;; sequentially in some order.
;; b. What are some other values that could be produced if the system allows
;; the processes to be interleaved?
;; Draw timing diagrams like the one in figure 3.29 to explain how these values
;; can occur.
;;
;; Answer to a.
;; Peter, Paul, Mary
;; (/ (+ 100 10 -20) 2)
;; 45
;; Peter, Mary, Paul
;; (- (/ (+ 100 10) 2) 20)
;; 35
;; Paul, Peter, Mary
;; (/ (+ 100 10 -20) 2)
;; 45
;; Paul, Mary, Peter
;; (+ (/ (- 100 20) 2) 10)
;; 50
;; Mary, Peter, Paul
;; (+ (/ 100 2) 10 -20)
;; 40
;; Mary, Paul, Peter
;; (+ (/ 100 2) -20 10)
;; 40
;;
;; 35, 40, 45, 50
;;
;; Answer to b.
;; People Time Balance
;; Peter 0 110
;; Paul  0 80
;; Mary  0 50
;;
;; Peter 0 110
;; Paul  1 90
;; Mary  1 50
;;
;; Paul  0 80
;; Peter 1 90
;; Mary  1 40
;;
;; Mary  0 50
;; Peter 1 60
;; Paul  1 30
;;
;; Peter 0 110
;; Paul  0 80
;; Mary  1 40 or 55
;;
;; Peter 0 110
;; Mary  0 50
;; Paul  1 90 or 30
;;
;; Paul  0 80
;; Mary  0 50
;; Peter 1 90 or 60
;; 
;; 30, 40, 50, 55, 60, 80, 90, 110
;;
;; Exercise 3.39.
;; Which of the five possibilities in the parallel execution
;; shown above remain if we instead
;; serialize execution as follows:
;;
;; (define x 10)
;; (define s (make-serializer))
;; (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
;;                   (s (lambda () (set! x (+ x 1)))))
;;
;; three events:
;; a. ((s (lambda () (* x x))))
;; b. (s (lambda () (set! x (+ x 1))))
;; c. (set! x a)
;;
;; c happens after a
;;
;; a c b
;; x is 101
;;
;; a b c
;; x is 100
;;
;; b a c
;; x is 121
;;
;; Exercise 3.40.
;; Give all possible values of x that can result from executing
;; (define x 10)
;; (parallel-execute (lambda () (set! x (* x x)))
;;                   (lambda () (set! x (* x x x))))
;; Which of these possibilities remain if we instead use serialized procedures:
;; (define x 10)
;; (define s (make-serializer))
;; (parallel-execute (s (lambda () (set! x (* x x))))
;;                   (s (lambda () (set! x (* x x x)))))
;;
;; three events in p1,
;; a. access x
;; b. access x
;; c. set! x
;;
;; (a b c)
;;
;; four events in p2,
;; w. access x
;; x. access x
;; y. access x
;; z. set! x
;;
;; (w x y z)
;; 1000000: a b c w x y z
;; 100: a b w x y z c
;; 1000: a b w x y c z 
;; 10000: a w x y z b c
;; 100000: w a b c x y z
;;
;; 1000000: a b c w x y z
;; 
;; because of commutative of multiply
;;
;; 3.41.
;;
;; I don't think there is need to serilize read access to balance.
;; Since balance is not modified during read.
;;
;; 3.42.
;;
;; I think it's safe.
;;
;; 3.43.
;;
;; The account could be 40 30 10 if arbitray order is allowed.
;;
;; 3.44.
;;
;; Louis is not right, since no difference needs to be computed between
;; from-account and to-account and individual account operations is valid
;; for both of them since they are serilized.
;;
;; 3.45.
;;
;; Wrong serializer could be used if doing Louis's way.
