;; Parsed expression datatypes
(define-datatype expression expression?
	[var-exp        ; variable references
		(id symbol?)]
	[lit-exp        ; "Normal" data.  Did I leave out any types?
		(datum
			(lambda (x)
				(ormap (lambda (pred) (pred x))
				(list number? vector? boolean? symbol? string? pair? null?))))]
	[app-exp        ; applications
		(rator expression?)
		(rands (lambda (x) (or (expression? x)((list-of expression?) x))))]  
	[quote-exp
		(datum always?)]
	[if-exp
		(test-exp expression?)
		(then-exp expression?)
		(else-exp always?)]
	[let-exp
		(vars (list-of expression?))
		(exps (list-of expression?))
		(bodies (list-of expression?))]
	[let*-exp
		(vars (list-of expression?))
		(exps (list-of expression?))
		(bodies (list-of expression?))]
	[lambda-exp
		(vars (lambda (x) (or (expression? x)((list-of expression?) x))))
		;(lambda (x) (or (symbol? x)((list-of symbol?) x)))
		(bodies (lambda (x) (or (expression? x)((list-of expression?) x))))]
	[lambda-exp-improper ;improper means (lambda (x y . z)
		(vars (list-of expression?))
		(extra-vars symbol?)
		(bodies (list-of expression?))]
	[begin-exp
		(exps (list-of expression?))]
	[set!-exp
		(var (expression?))
		(ex (lambda (x) (or (expression? x)((list-of expression?) x))))]
	[cond-exp
		(exps (list-of expression?))]
	[while-exp
		(test-exp (lambda (x) (or (expression? x)((list-of expression?) x))))
		(then-exp expression?)]
	[case-exp
		(eva (lambda (x) (or (expression? x)((list-of expression?) x))))
		(cases (list-of expression?))]
	[keys-list-exp
		(keys (list-of always?))]
	[and-exp 
		(exps (list-of expression?))]
	[or-exp 
		(exps (list-of expression?))]
	[letrec-exp
		(proc-names (list-of symbol?))
		(idss (lambda (x) (or (list-of (list-of symbol?)))))
		(bodies (list-of expression?))
		(letrec-body (lambda (x) (or (expression? x)((list-of expression?) x))))]
	[letrec-exp-improper
		(proc-names (list-of symbol?))
		(idss (lambda (x) (or (list-of (list-of symbol?)))))
		(bodies (list-of expression?))
;		(improper (list-of list?))
;		(proper (list-of list?))
		(letrec-body (lambda (x) (or (expression? x)((list-of expression?) x))))]
)
	
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?))
  (recursively-extended-env-record
	(proc-names (list-of symbol?))
	(idss (lambda (x) (or (list-of (list-of symbol?))((list-of pair?) x))))
	(bodies (list-of expression?))
	(env environment?))
  (recursively-extended-env-record-improper
	(proc-names (list-of symbol?))
	(idss (lambda (x) (or (list-of (list-of symbol?))((list-of pair?) x))))
	(bodies (list-of expression?))
	(env environment?)))
	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
	(vars   (lambda (x) (or (symbol? x)((list-of symbol?) x)(pair? x))) )
	(bodies (list-of expression?))
	(env environment?)]
   [closure-improper
	 (vars (list-of symbol?))
	 (improper-var symbol?)
	 (bodies (list-of expression?))
	 (env environment?)]
)
	
;; environment type definitions
(define scheme-value?
  (lambda (x) #t))