(define (error msg info) msg)

(define (primitive-procedure? procedure)
  (+ 1 1))
(define (apply-primitive-procedure procedure arguments)
  (+ 1 1))
(define (compound-procedure? procedure)
  (+ 1 1))
(define (eval-sequence proc env)
  (+ 1 1))
(define (procedure-body procedure)
  (+ 1 1))
(define (extend-environment parameters arguemnts env)
  (+ 1 1))
(define (procedure-parameters procedure)
  (+ 1 1))
(define (procedure-environment procedure)
  (+ 1 1))

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
	 (error "Uknown procedure type --APPLY" procedure))))

(define (no-operands? exps)
  (+ 1 1))
(define (first-operand exps)
  (+ 1 1))
(define (rest-operands exps)
  (+ 1 1))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))


(define (operator exp)
  (+ 1 1))
(define (operands exp)
  (+ 1 1))

"****eval-assignment-type called in eval begin****"
(define (assignment-variable exp)  (cadr exp))
(define (assignment-value exp)  (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok);what's this?
"****eval-assignment-type called in eval end****"

"****eval-definition-type called in eval begin****"
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))xs
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caddr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
		    (eval (definition-value exp) env)
		    env)
  'ok)
"****eval-definition-type called in eval end****"

(define (lambda-parameters exp)  (cadr exp))
(define (lambda-body exp)  (cddr exp))




"***eval-begin-type called in eval begin***"
(define (begin-actions exps) (cdr exps))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp exps) (car exps))
(define (rest-exps exps) (cdr exps))
  
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))
"***eval-begin-type called in eval end***"

"***eval-if-type(cond) called in eval begin***"
(define (if-predicate   exp) (cadr exp))
(define (if-consequent  exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false));should be '#f
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (cond-actions clause) (cdr clause))
(define (cond-predicate clause) (car clause))
(define (cond-esle-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-clauses exp) (cdr exp))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (null? rest)
	    (sequence->exp (cond-actions first))
	    (error "ELSE clause isn't  last --COND->IF" clauses))
	(make-if (cond-predicate first)
		 (sequence->exp (cond-actions first))
		 (expand-clauses rest)))))
"***eval-if-type(cond) called in eval end***"

"****called in types of exp begin****"
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
"****called in types of exp end****"

"***types of exp called in eval begin***"
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))
(define (variable?    exp) (symbol? exp))
(define (quoted?      exp) (tagged-list? exp 'quote))
(define (assignment?  exp) (tagged-list? exp 'set!))
(define (definition?  exp) (tagged-list? exp 'define))
(define (lambda?      exp) (tagged-list? exp 'lambda))
(define (if?          exp) (tagged-list? exp 'if))
(define (cond?        exp) (tagged-list? exp 'cond))
(define (begin?       exp) (tagged-list? exp 'begin))
(define (application? exp) (pair? exp))
"***type of exp called in eval end***"

"**main eval begin**"
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable?   exp) (lookup-variable-value exp env))
	((quoted?     exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if?         exp) (eval-if exp env))
	((cond?       exp) (eval-if (cond->if exp) env))
	((begin?      exp) (eval-sequence (begin-actions exp) env))
	((lambda? exp) (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type --EVAL" exp))))
"**main eval end**"

"eval number"
(eval 123 1)
"eval string"
(eval "abc" 1)
"eval 
