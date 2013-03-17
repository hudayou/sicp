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
    (cons (make-lambda (let-vars exp)
                       (let-body exp))
          (let-inits exp)))

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
