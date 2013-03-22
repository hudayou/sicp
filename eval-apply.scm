(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp)
         (eval-and exp env))
        ((or? exp)
         (eval-or exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "unknown expression type -- eval" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "unknown procedure type -- apply" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; solution for 4.1
;; given the fact first-operand is the leftmost operand
;; eval operands from left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-value (eval (first-operand exps) env))
          (rest-value (list-of-values (rest-operands exps) env)))
      (cons first-value rest-value))))
;; eval operands from right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((rest-value (list-of-values (rest-operands exps) env))
          (first-value (eval (first-operand exps) env)))
      (cons first-value rest-value))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)    ; formal parameters
                 (cddr exp))))  ; body
(define (make-definition name formal-parameters body)
  (cons 'define (cons (cons name formal-parameters) body)))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-test-recipient-clause? clause)
  (eq? (first-action (cond-actions clause)) '=>))
(define (first-action actions)
  (car actions))
(define (rest-actions actions)
  (cdr actions))
(define (cond-recipient test-recipient-clause)
  (rest-actions (cond-actions test-recipient-clause)))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false ; no else clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "else clause isn't last -- cond->if"
                 clauses))
        (if (cond-test-recipient-clause? first)
          (make-if (cond-predicate first)
                   (test-recipient->exp (cond-predicate first)
                                        (cond-recipient first))
                   (expand-clauses rest))
          (make-if (cond-predicate first)
                   (sequence->exp (cond-actions first))
                   (expand-clauses rest)))))))

(define (test-recipient->exp test recipient)
  (list recipient test))

;; solution for 4.2
;; a. special forms should take precedence over odinary procedures
;; b. (define (application? exp) (tagged-list? exp 'call))
;;    (define (operator exp) (cadr exp))
;;    (define (operands exp) (cddr exp))
;;    (define (no-operands? ops) (null? ops))
;;    (define (first-operand ops) (car ops))
;;    (define (rest-operands ops) (cdr ops))

(define (predicates exp)
  (cdr exp))

(define (no-predicate? preds)
  (null? preds))

(define (last-predicate? preds)
  (null? (cdr preds)))

(define (first-predicate preds)
  (car preds))

(define (rest-predicates preds)
  (cdr preds))

(define (eval-and-predicates preds env)
  (cond ((no-predicates? preds) 'true)
        ((last-predicate? preds) (eval (first-predicate preds)))
        ((true? (eval (first-predicate preds) env))
         (eval-and-predicates (rest-predicates preds) env))
        (else
          'false)))

(define (eval-and exp env)
  (eval-and-predicates (predicates exp) env))

(define (eval-or-predicates preds env)
  (cond ((no-predicates? preds) 'false)
        ((false? (eval (first-predicate preds) env))
         (eval-or-predicates (rest-predicates preds) env))
        (else
          (eval (first-predicate preds) env))))

(define (eval-or exp env)
  (eval-or-predicates (predicates exp) env))

(define (and->if exp)
  (expand-and-predicates (predicates exp)))

(define (expand-and-predicates preds)
  (if (null? preds)
    'true
    (make-if (first-predicate preds)
             (expand-and-predicates (rest-predicates preds))
             'false)))

(define (or->if exp)
  (expand-or-predicates (predicates exp)))

(define (expand-or-predicates preds)
  (if (null? preds)
    'false
    (make-if (first-predicate preds)
             'true
             (expand-or-predicates (rest-predicates preds)))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars exp) (map car (let-bindings exp)))
(define (let-inits exp) (map cadr (let-bindings exp)))

(define (make-let binding body)
  (list 'let (list binding) body))

(define (let->combination exp)
  (if (named-let? exp)
    (named-let->combination exp)
    (cons (make-lambda (let-vars exp)
                       (let-body exp))
          (let-inits exp))))

;; transform named let to letrec, let or internal definition
;;
;; (define (fib n)
;;   (let fib-iter ((a 1)
;;                  (b 0)
;;                  (count n))
;;     (if (= count 0)
;;       b
;;       (fib-iter (+ a b) a (- count 1)))))
;;
;; (define (fib n)
;;   (letrec ((fib-iter (lambda (a b count)
;;                        (if (= count 0)
;;                          b
;;                          (fib-iter (+ a b) a (- count 1))))))
;;     (fib-iter 1 0 n)))
;;
;; ;; http://en.wikipedia.org/wiki/Lambda_calculus#Recursion_and_fixed_points
;; (define (fib n)
;;   (let ((fib-iter (lambda (fib-iter a b count)
;;                     (if (= count 0)
;;                       b
;;                       (fib-iter fib-iter (+ a b) a (- count 1))))))
;;     (fib-iter fib-iter 1 0 n)))
;;
;; (define (fib n)
;;   (let ((fib-iter (lambda ()
;;                     (define (fib-iter a b count)
;;                       (if (= count 0)
;;                         b
;;                         (fib-iter (+ a b) a (- count 1))))
;;                     (fib-iter 1 0 n))))
;;     (fib-iter)))

(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))
(define (named-let-vars exp) (map car (named-let-bindings exp)))
(define (named-let-inits exp) (map cadr (named-let-bindings exp)))

(define (named-let->combination exp)
  (define (make-body definition body)
    (list definition body))
  (let ((name (named-let-name exp))
        (vars (named-let-vars exp))
        (inits (named-let-inits exp))
        (body (named-let-body exp)))
    (make-let (list name (make-lambda '()
                                      (make-body
                                        (make-definition name vars body)
                                        (cons name inits))))
              (list name))))

(define (make-named-let name bindings body)
  (list 'let name bindings body))

;; transform let* to nested lets
;;
;; (let* ((x 3)
;;        (y (+ x 2))
;;        (z (+ x y 5)))
;;   (* x z))
;;
;; (let ((x 3))
;;   (let ((y (+ x 2)))
;;     (let ((z (+ x y 5)))
;;       (* x z))))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (last-binding? bindings) (null? (cdr bindings)))
  (define (first-binding bindings) (car bindings))
  (define (rest-bindings bindings) (cdr bindings))
  (define (iter-bindings bindings body)
    (if (last-binding? bindings)
      (cons 'let (cons (list (first-binding bindings))
                       body))
      (make-let (first-binding bindings)
                (iter-bindings (rest-bindings bindings) body))))
  (iter-bindings (let-bindings exp)
                 (let-body exp)))

;; do body if predicates are not true
;; (do bindings predicates body)
;;
;; (do ((var init [step]) ...) (test [expr ...]) body)

;; transform do to named-let
;;
;; (do ((i 1 (1+ i)))
;;   ((> i 4))
;;   (display i))
;;
;; (do ((i 1 (1+ i))
;;      (p 3 (* 3 p)))
;;     ((> i 4)
;;      p)
;;   (format #t "3**~s is ~s\n" i p))
;;
;; (let do ((i 1)
;;          (p 3))
;;   (if (> i 4)
;;     (begin
;;       p)
;;     (begin
;;       (format #t "3**~s is ~s\n" i p)
;;       (do (1+ i) (* 3 p)))))
;;
;; (define d1 '(do ((i 1 (1+ i)) (p 3 (* 3 p))) ((> i 4) p) (format #t "3**~s
;; is ~s\n" i p)))
;;
;; (define d2 '(do ((i 1 (1+ i))) ((> i 4)) (display i)))

(define (do? exp) (tagged-list? exp 'do))
(define (do-bindings exp) (map (lambda (x) (list-head x 2))
                               (cadr exp)))
(define (do-test exp) (caaddr exp))
(define (do-test-exps exp) (cdaddr exp))
(define (no-exps-after-test? exp) (null? (cdaddr exp)))
(define (do-body exp) (cdddr exp))
(define (do-steps exp) (map (lambda (x) (if (null? (list-tail x 2))
                                          (car x)
                                          (caddr x)))
                            (cadr exp)))

(define (do->named-let exp)
  (make-named-let
    'do
    (do-bindings exp)
    (if (no-exps-after-test? exp)
      (make-if (do-test exp)
               'true ;; fixme, how to return unspecified value?
               (sequence->exp (append (do-body exp)
                                      (list
                                        (cons 'do
                                              (do-steps exp))))))
      (make-if (do-test exp)
               (sequence->exp (do-test-exps exp))
               (sequence->exp (append (do-body exp)
                                      (list
                                        (cons 'do
                                              (do-steps exp)))))))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (remove-binding-from-frame! var frame)
  (define (remove-binding! prev-vars vars prev-vals vals)
    (if (null? vars)
      'done
      (if (eq? var (car vars))
        (set-cdr! pre-vars (cdr vars))
        (remove-binding! vars (cdr vars) vals (cdr vals)))))
  (let ((vars (frame-variables frame))
        (vals (frame-values frame)))
    (if (eq? var (car vars))
      (begin
        (set-car! frame (cdr vars))
        (set-cdr! frame (cdr vals)))
      (remove-binding! vars (cdr vars) vals (cdr vals)))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "too many arguments supplied" vars vals)
      (error "too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "unbound variable -- set!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; represent a frame as a list of bindings,
;; where each binding is a name-value pair

(define (make-frame variables values)
  (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var val)))

(define (env-loop env loop set error-msg var)
  (if (eq? env the-empty-environment)
    (error error-msg var)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((null? vars)
               (if loop
                 (env-loop (enclosing-environment env))
                 (add-binding-to-frame! var val frame)))
              ((eq? var (car vars))
               (if set
                 (set-car! vals val)
                 (car vals)))
              (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame)))))

(define (lookup-variable-value var env)
  (env-loop env true false "unbound variable" var))

(define (set-variable-value! var val env)
  (env-loop env true true "unbound variable -- set!" var))

(define (define-variable! var val env)
  (env-loop env true true "empty environment" var))

(define (make-unbound! var env)
  (define (env-loop env)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars))
               (remove-binding-from-frame! var frame)
               (env-loop (enclosing-environment env)))
              (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame))))
  (env-loop env))
