(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;; compound expressions
        (else
          ((get (type-tag exp) 'eval) (contents exp) env))))

(define (type-tag exp)
  (car exp))

(define (contents exp)
  (cdr exp))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (install-quoted-package)
  (define (eval-quotation exp) (cadr exp))
  (put 'quoted 'eval
       (lambda (exp env)
         (eval-quotation (attach-tag 'quoted exp) env)))
  'done)

(define (install-assignment-package)
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)
  (put 'set! 'eval
       (lambda (exp env)
         (eval-assignment (attach-tag 'set! exp) env)))
  'done)

(define (install-definition-package)
  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)    ; formal parameters
                   (cddr exp))))  ; body
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env)
                      env)
    'ok)
  (put 'define 'eval
       (lambda (exp env)
         (eval-definition (attach-tag 'define exp) env)))
  'done)

(define (install-if-package)
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
  (put 'if 'eval
       (lambda (exp env)
         (eval-if (attach-tag 'if exp) env)))
  'done)

(define (install-lambda-package)
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
  (put 'lambda 'eval
       (lambda (exp env)
         (eval-lambda (attach-tag 'lambda exp) env)))
  'done)

(define (install-begin-package)
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))
  (define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))
  (put 'begin 'eval
       (lambda (exp env)
         (eval-sequence exp env)))
  'done)

(define (install-cond-package)
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))
  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  (define (make-begin seq) (cons 'begin seq))
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
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
          (make-if (cond-predicate first)
                   (sequence->exp (cond-actions first))
                   (expand-clauses rest))))))
  (put 'cond 'eval
       (lambda (exp env)
         (eval (cond->if (attach-tag 'cond exp) env))))
  'done)

(define (install-procedure-package)
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))
  (define (list-of-values exps env)
    (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
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
  (define (eval-procedure exp env)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))
  (put 'call 'eval eval-procedure)
  'done)
