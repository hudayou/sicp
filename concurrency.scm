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
