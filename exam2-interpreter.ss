;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

(load "chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+
(define lambda-var?
	(lambda (x)
		(or (symbol? x)
			(null? x)
			(pair? x))))

; parsed expression

(define-datatype expression expression?
	[var-exp
    	(id symbol?)]
	[lit-exp
	    (id 
			(lambda (x) 
				(or (boolean? x) 
				(number? x) 
				(string? x) 
				(vector? x)
				(symbol? x)
				(pair? x)
				(null? x))))]
	[lambda-exp
	    (var (list-of symbol?))
    	(body (check-eles-in-list expression?))]
	[lambda-symbol-exp
	    (var symbol?)
    	(body (check-eles-in-list expression?))]
	[lambda-improper-exp
		(var improper?)
    	(body (check-eles-in-list expression?))]
    [let-exp
      	(var (list-of symbol?))
      	(val (list-of expression?))
      	(body (check-eles-in-list expression?))]
	[named-let-exp
      	(name symbol?)
      	(val expression?)
      	(body (check-eles-in-list expression?))]
    [if-else-exp
      	(test-exp expression?)
      	(true-exp expression?)
      	(false-exp expression?)]
	[if-without-else-exp
      	(test-exp expression?)
      	(true-exp expression?)]
	[set!-exp
    	(var symbol?)
    	(body expression?)]
	[app-exp
		(rand expression?)
    	(rator (lambda (x) (map expression? x)))]
	[or-exp
		(var (list-of expression?))]
	[and-exp
		(var (list-of expression?))]
	[cond-exp
		(tests (list-of expression?))
		(bodies (list-of expression?))]
	[begin-exp
		(bodies (list-of expression?))]
	[case-exp
		(test expression?)
		(comparators (list-of expression?))
		(bodies (list-of expression?))]
	[while-exp
		(test expression?)
		(bodies (list-of expression?))]
	[do1-exp
		(bodies (list-of expression?))
		(test expression?)]
	[do2-exp
		(bodies (list-of expression?))
		(test expression?)]
	[call-with-values-exp
		(producer expression?)
		(consumer expression?)])

	
	

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))
  
(define list-of
	(lambda (pred)
		(lambda (ls)
			(if (list? ls)
				(andmap pred ls)
				#f))))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (lambda (x) 
			(or ((list-of symbol?) x)
				(improper? x))))
   (vals (list-of scheme-value?))
   (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
   [closure 
	(params (list-of symbol?))
	(bodies (list-of expression?))
	(env environment?)]
	[closure-imp 
		(params improper?)
		(bodies (list-of expression?))
		(env environment?)]
	[closure-sym 
		(params symbol?)
		(bodies (list-of expression?))
		(env environment?)])
	 
	

;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define 2-lists?
  (lambda (lst)
    (if (null? lst)
        #t
        (if (and (list? (car lst)) (eq? (length (car lst)) 2))
            (2-lists? (cdr lst))
            #f))))
		
(define check-eles-in-list 					
	(lambda (pred)
		(lambda (val)
			(or (null? val)
				(and (list? val) (pred (car val)) ((check-eles-in-list pred) (cdr val)))))))
			
(define check-eles-in-set
	(lambda (exp)
		(lambda (datum)
			(and ((check-eles-in-list exp) datum) 
  			(let loop ([datum datum])
  				(if (null? datum)
      			#t
      			(if (member (car datum) (cdr datum))
       		 		#f
             	(loop (cdr datum)))))))))
				
(define split-list	
	(lambda(ls)
		(list (parse-exp (car ls))(parse-exp (cadr ls)))))
		
(define unsplit-list	
	(lambda(ls)
		(list (unparse-exp (car ls))(unparse-exp (cadr ls)))))
		
(define improper? 
	(lambda (ls)
		(let check ([rest ls])
			(cond [(null? rest) #t]
				[(symbol? rest) #t]
				[else (and (symbol? (car rest)) (check (cdr rest)))]))))
				
(define total-imp-params
	(lambda (ls acc)
		(if (or (null? (cdr ls))(symbol? (cdr ls)))
			acc
			(total-imp-params (cdr ls) (+ acc 1)))))
			
(define map-ordered
	(lambda (proc exp)
		(let helper ([e exp])
			(if (null? e)
				(list)
				(cons (proc (car e)) (helper (cdr exp)))))))

(define parse-exp
	(lambda (datum)
		(cond 
			[(symbol? datum) 
				(if (eq? 'else datum)
					(lit-exp datum)
				(var-exp datum))]
				
			[(or (number? datum) (boolean? datum) (string? datum) (vector? datum) (null? datum) (null? datum) (symbol? datum) 
				(and (list? datum) (list? (cdr datum)) (eqv? 'quote (1st datum))))
				(if (and (list? datum) (list? (cdr datum)) (eqv? 'quote (1st datum)))
					(lit-exp (2nd datum))
					(lit-exp datum))]
			
			[(and (pair? datum) (not (list? datum)))
				(eopl:error 'parse-exp "bad expression: ~s" datum)]
			
			[(pair? datum)
			 	(cond
					
					[(eq? 'cond (car datum)) ;cond
						(cond 
							[(or (null? (cdr datum))(andmap (lambda (x) (not (pair? x))) (cdr datum))
									(ormap null? (map cdr (cdr datum))))
									(eopl:error 'parse-exp "Found empty cond expression: ~s" datum)]
							[else 
								(let ([test-exps (map car (cdr datum))]
									[list-of-bodies ((lambda (x) (map parse-exp x)) (map cadr (cdr datum)))])
										(if (= (or (list-index (lambda (e) (eq? e 'else)) test-exps) (- (length list-of-bodies) 1)) (- (length list-of-bodies) 1))
											
												(cond-exp (map parse-exp test-exps) list-of-bodies)
											(eopl:error 'parse-exp "Found an else not at the end of a cond expression: ~s" datum)))])]
					[(eq? 'case (car datum)) ;case
						(cond 
							[(or (null? (cdr datum))(andmap (lambda (x) (not (pair? x))) (cdr datum))
									(andmap (lambda (x) (not (pair? x))) (cddr datum))
									(ormap null? (map cdr (cdr datum))))
									(eopl:error 'parse-exp "Found empty case expression: ~s" datum)]
							[else 
								(let ([test-exps (cadr datum)]
									[comps (map car (cddr datum))]
									[list-of-bodies ((lambda (x) (map parse-exp x)) (map cadr (cddr datum)))])
										(if (= (or (list-index (lambda (e) (eq? e 'else)) comps) (- (length list-of-bodies) 1)) (- (length list-of-bodies) 1))
												(case-exp (parse-exp test-exps) (map (lambda (x) 
																					 (if (eqv? 'else x)
																					 	  (lit-exp 'else)
																					 	  (lit-exp x))) 
																					 comps) 
																				list-of-bodies)
											(eopl:error 'parse-exp "Found an else not at the end of a case expression: ~s" datum)))])]
					[(eq? 'begin (car datum)) ;begin
						(cond 
							[(null? (cdr datum))
								(eopl:error 'parse-exp "found an empty begin: ~s" datum)]
							[else 
								(begin-exp (map parse-exp (cdr datum)))])]
					[(eq? 'and (car datum)) (if (null? (cdr datum)) ;and
							(and-exp (list (lit-exp #t)))
							(and-exp (map parse-exp (cdr datum))))]
					[(eq? 'or (car datum))  ;or
						(if (null? (cdr datum))
							(or-exp (list (lit-exp #f)))
							(or-exp (map parse-exp (cdr datum))))]
					[(eq? 'while (car datum)) ;while
						(cond 
							[(or (null? (cdr datum)) (null? (cddr datum)))
								(eopl:error 'parse-exp "No test/body expressions found in a while expression: ~s" datum)]
							[else
								(while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]

					[(eq? 'do1 (car datum))	;do1
						(cond [(or (null? (cdr datum)) (null? (cddr datum)))
								(eopl:error 'parse-exp "No test/body expressions found in a do1 expression: ~s" datum)]
							  [(not (eq? 'while (3rd datum)))
							  	(eopl:error 'parse-exp "syntax error in a do1 expression: ~s" datum)]
							  [else (do1-exp (map parse-exp (cadr datum)) (parse-exp (cadddr datum)))])]

					[(eq? 'do2 (car datum))	;do2
						(cond [(or (null? (cdr datum)) (null? (cddr datum)))
								(eopl:error 'parse-exp "No test/body expressions found in a do1 expression: ~s" datum)]
							  [(not (eq? 'while (3rd datum)))
							  	(eopl:error 'parse-exp "syntax error in a do1 expression: ~s" datum)]
							  [else (do2-exp (map parse-exp (cadr datum)) (parse-exp (cadddr datum)))])]

					[(eq? 'call-with-values (car datum)) ;call-with-values
						(cond [call-with-values-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))])]
							
	   	 		 	[(eq? (1st datum) 'lambda);lambda
	   	  				(if (< (length (cdr datum)) 2)
	   	      			(eopl:error 'parse-exp "Error in parse-exp: lambda: incorrect length: " datum)
			  			  	(cond																							
	   	        				[(and (list? (2nd datum)) 
									((check-eles-in-set symbol?) (2nd datum)))
									(lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
												
	   	        				[(symbol? (2nd datum)) 
									(lambda-symbol-exp (2nd datum) (map parse-exp (cddr datum)))]
								[(improper? (2nd datum))
									(lambda-improper-exp (2nd datum) (map parse-exp (cddr datum)))]
	   	        				[(not(list? (2nd datum)))
									(lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
	   	        				[else
					                (eopl:error 'parse-exp "Error in parse-exp: lambda argument list: formals must be symbols: " datum)]))]
									
		   			[(and (eq? (1st datum) 'let);named-let
						(symbol? (2nd datum)))
		         	(if (not (> (length datum) 1))
						(eopl:error 'parse-exp "Error in parse-exp: named-let: incorrect length: " datum)
					 	(cond
							[(and (andmap list? (3rd datum))
								((check-eles-in-set symbol?) (map car (3rd datum))))
									(named-let-exp (2nd datum) 
										(parse-exp (3rd datum)) 
										(map parse-exp (cdddr datum)))]
							[else
								(eopl:error 'parse-exp "Error in parse-exp: named-let:" datum)]))]
					
						[(or (and (eq? (1st datum) 'let) (list? (2nd datum)));let
							(eq? (1st datum) 'let*);let*
							(eq? (1st datum) 'letrec));letrec
							(if (not (> (length datum) 2))
								(eopl:error 'parse-exp "Error in parse-exp: let/let*/letrec: incorrect length: " datum)
		   	  					(cond
									[(and (list? (2nd datum))
										(2-lists? (2nd datum))
										((check-eles-in-set symbol?) (map car (2nd datum))))
										(let-exp (map car (2nd datum))(map parse-exp (map cadr (2nd datum))) 
										(map parse-exp (cddr datum)))]
									[else
										(eopl:error 'parse-exp "Error in parse-exp: let argument list: formals must be symbols:" datum)]))]
					
						[(eq? (1st datum) 'if);if
							(cond
								[(eq? (length (cdr datum)) 3);if-else
									(if-else-exp (parse-exp (2nd datum)) 
									 	(parse-exp (3rd datum)) 
									 	(parse-exp (4th datum)))]
			   	    		
								[(eq? (length (cdr datum)) 2);if-without-else
									(if-without-else-exp (parse-exp (2nd datum)) 
									(parse-exp (3rd datum)))]
		   	    		
								[else
									(eopl:error 'parse-exp "Error in parse-exp: if: missing then and else parts: " datum)])]
								
			[(eq? (1st datum) 'set!)
			  	(cond
					[(> (length datum) 3)
						(eopl:error 'parse-exp "Error in parse-exp: set!: Too many parts: " datum)]
					[(< (length datum) 2)
						(eopl:error 'parse-exp "Error in parse-exp: set!: missing parts: " datum)]
					[(and (eq? (length datum) 3) 
			  			(symbol? (2nd datum)))					        		 
							(set!-exp (2nd datum) 
							(parse-exp (3rd datum)))]
					[else
						(eopl:error 'parse-exp "Error in parse-exp: set!: " datum)])]
			[else 
						(app-exp (parse-exp (1st datum)) 
										 (map parse-exp (cdr datum)))])]
		     
			 [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))
			 
 
(define unparse-exp 
	(lambda (exp)
    	(cases expression exp
			[var-exp (id) id]
			[lit-exp (id) id]
      		[lambda-exp (var body) 
				(append (list 'lambda var)
				(map unparse-exp body))]      		
			[let-exp (var val body)
				(append (list var (unparse-exp val)) 
			   		   	(map unparse-exp body))]
			[named-let-exp (name val body)
				(append (list 'let name (unparse-exp assign)) 
							 	(map unparse-exp body))]
			[if-else-exp (test-exp true-exp false-exp)
					(list 'if 
					  	(unparse-exp test-exp) 
						(unparse-exp true-exp) 
						(unparse-exp false-exp))]
			[if-without-else-exp (test-exp true-exp)
					(list 'if 
						(unparse-exp test-exp)
						(unparse-exp true-exp))]
			[set!-exp (var body)
					(list 'set! 
						var 
						(unparse-exp body))]
			[app-exp (rand rator)
					(cons (unparse-exp rand)
        				(map unparse-exp rator))])))








;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail)))))))








;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+



; To be added later


(define syntax-expand
 	(lambda (exp)
 		(cases expression exp
			[if-without-else-exp (test-exp true-exp)
 				(if-without-else-exp (syntax-expand test-exp) (syntax-expand true-exp))]
 			[if-else-exp (test-exp true-exp false-exp)
 				(if-else-exp (syntax-expand test-exp) (syntax-expand true-exp) (syntax-expand false-exp))]
 			[lambda-exp (var body)
 				(lambda-exp var (map syntax-expand body))]
			[lambda-symbol-exp (var body)
				(lambda-symbol-exp var (map syntax-expand body))]
			[lambda-improper-exp (var body)
				(lambda-improper-exp var (map syntax-expand body))]
 			[let-exp (var val body)
 				(app-exp(lambda-exp var (map syntax-expand body)) (map syntax-expand val))]
 			[named-let-exp (name val body)
				(named-let-exp (syntax-expand name) (syntax-expand val) (map syntax-expand body))]
 			[set!-exp (var body)
 				(set!-exp (syntax-expand var) (syntax-expand body))]
 			[app-exp (rand rator)
 				(app-exp (syntax-expand rand) (map syntax-expand rator))]
			[or-exp (var) 
				(let help ([val var])
					(cond [(null? val) (lit-exp #f)]
						[(null? (cdr val)) (syntax-expand (car val))]
						[else 
							(app-exp
								(lambda-exp 
									(list 'x)
									(list 
										(if-else-exp 
											(var-exp 'x)
											(var-exp 'x)
											(help (cdr val))))) 
								(list (syntax-expand (1st val))))]))]
			[and-exp (var) 
				(let help ([val var])
					(cond [(null? val) (lit-exp #t)]
						[(null? (cdr val)) (syntax-expand (car val))]
						[else 
							(app-exp
								(lambda-exp 
									(list 'x)
									(list 
										(if-else-exp 
											(var-exp 'x)
											(help (cdr val))
											(var-exp 'x)))) 
								(list (syntax-expand (1st val))))]))]
			[cond-exp (tests bodies)
				(let help ([tes tests][val bodies])
					(cond 
						[(null? tes) (parse-exp '(void))]
						[(eq? (car tes) 'else) (syntax-expand (car val))] 
						[else(app-exp
							(lambda-exp 
								(list 'x)
								(list 
									(if-else-exp 
										(var-exp 'x)
										(syntax-expand (car val))
										(help (cdr tes) (cdr val))))) 
							(list (syntax-expand (1st tes))))]))]
			[case-exp (tests comparators bodies) 
					(let helper ([test tests] [comp comparators] [val bodies])
						(cond [(null? comp) (parse-exp 'void)]
							  [(eqv? (cadar comp) (quote else))
							  			(syntax-expand (car val))]
							  [else (if-else-exp
							  		(app-exp (var-exp 'not) (list (app-exp (var-exp 'member) (list test (car comp)))))
							  				(helper test (cdr comp) (cdr val))
							  				(syntax-expand (car val)))]))]
			[while-exp (test bodies)
				(while-exp (syntax-expand test) (map syntax-expand bodies))]
			[begin-exp (bodies)
				(app-exp (lambda-exp '() (map syntax-expand bodies)) (list))]
			[do1-exp (bodies test)	;do1
				(begin (map syntax-expand bodies)
					   (while-exp test bodies))]

			[call-with-values-exp (producer consumer)
				(lambda-exp (syntax-expand producer) (syntax-expand consumer))]
			[else exp])))






;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+


(define *prim-proc-names* '(+ - * / not add1 sub1 zero? not < > >= <= = cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list null? assq eq? equal? 
atom? length list->vector list? pair? procedure? vector->list vector
make-vector vector-ref vector? number? symbol? set-car!  set-cdr!
vector-set! display newline quote apply map quotient void member values))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env))) 

(define global-env init-env)

; top-level-eval evaluates a form in the global environment

(define eval-bodies
	(lambda (bodies env)
		(let loop ([bodies bodies])
			(if (null? (cdr bodies))
				(eval-exp (car bodies) env)
				(begin 
					(eval-exp (car bodies) env)
					(loop (cdr bodies)))))))

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
  (let ([identity-proc (lambda (x) x)])
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
	  [let-exp (var val body) 
			(eval-bodies body 
				(extend-env var 
				    (eval-rands val env) 
				        env))]
	  [if-without-else-exp (test-exp true-exp)
	  		(if (eval-exp test-exp env)
	  			(eval-exp true-exp env))]
	  [if-else-exp (test-exp true-exp false-exp)
	  		(if (eval-exp test-exp env)
	  			(eval-exp true-exp env)
	  			(eval-exp false-exp env))]
	  [lambda-exp (var body) (closure var body env)]
	  [lambda-symbol-exp (var body) (closure-sym var body env)]
	  [lambda-improper-exp (var body) (closure-imp var body env)]
	  [while-exp (test bodies)
	  	(letrec ([helper (lambda ()
	  						(if (eval-exp test env)
	  							(begin (eval-bodies bodies env)
	  								   (helper))))])
	  			(helper))]
      [var-exp (id)
		(apply-env env id ; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () 
				(apply-env global-env
				id
				(lambda (x) x)
				(lambda ()
					(eopl:error 'apply-env ; procedure to call if id not in env
						"variable not found in environment: ~s"
						id)))))] 
	  [do2-exp (bodies test)
	  	(letrec ([helper (lambda ()
	  						(begin (eval-bodies bodies env)
	  								(if (eval-exp test env)
	  									(helper))))])
	  			(helper))]

	  ; [call-with-values-exp (producer consumer)
	  ; 	 (apply-proc consumer (eval-exp producer env))]
	  ;add or and case cond
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
	  ;closure
	  [closure (params bodies env)
	  	(eval-bodies bodies
	  				 (extend-env params args env))]
	  [closure-imp (params bodies env)
	  	(eval-bodies bodies
	  				 (extend-env (package-imp-args params (total-imp-params params 1)) (package-imp-args args (total-imp-params params 1)) env))]
	  [closure-sym (params bodies env)
		(eval-bodies bodies
					(extend-env (list params) (list args) env))]
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))
					
(define package-imp-args
	(lambda (ls num)
		(if (>= num 1)
			(cons (car ls) (package-imp-args (cdr ls) (- num 1)))
			(list ls))))


; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (apply = args)]
	  [(>) (apply > args)]
	  [(<) (apply < args)]
	  [(>=) (apply >= args)]
	  [(<=) (apply <= args)]
	  [(list) args]
	  [(null?) (null? (1st args))]
	  [(assq) (assq (1st args) (2nd args))]
	  [(eq?) (eq? (1st args) (2nd args))]
	  [(equal?) (equal? (1st args) (2nd args))]
	  [(atom?) (atom? (1st args))]
	  [(length) (length (1st args))]
	  [(list->vector) (list->vector (1st args))]
	  [(list?) (list? (1st args))]
	  [(pair?) (pair? (1st args))]
	  [(procedure?) (proc-val? (1st args))]
	  [(vector->list) (vector->list (1st args))]
	  [(vector) (apply vector args)]
	  [(make-vector) (if (null?(cdr args))
						(make-vector (1st args))
						(make-vector (1st args) (2nd args)))]
	  [(vector-ref) (vector-ref (1st args) (2nd args))]
	  [(vector?) (vector? (1st args))]
	  [(number?)(number? (1st args))]
	  [(symbol?) (symbol? (1st args))]
	  [(set-car!)(set-car! (1st args) (2nd args))]
	  [(set-cdr!)(set-cdr! (1st args) (2nd args))]
	  [(vector-set!)(vector-set! (1st args) (2nd args) (3rd args))]
	  [(display) (display (1st args))]
	  [(newline) (newline (1st args))]
	  [(car) (car (1st args))]
	  [(cdr) (cdr (1st args))]
	  [(caar) (caar (1st args))]
	  [(cadr) (cadr (1st args))]
	  [(cdar) (cdar (1st args))]
	  [(cddr) (cddr (1st args))]
	  [(caaar) (caaar (1st args))]
	  [(caadr) (caadr (1st args))]
	  [(cadar) (cadar (1st args))]
	  [(caddr) (caddr (1st args))]
	  [(cdaar) (cdaar (1st args))]
	  [(cdadr) (cdadr (1st args))]
	  [(cddar) (cddar (1st args))]
	  [(cdddr) (cdddr (1st args))]
	  [(quote) (1st args)]
	  [(member) (member (1st args) (2nd args))]
	  [(apply) (let ([args-ls 
	  					(let apply-helper ([args (cdr args)])
	  						(cond [(null? args) (eopl: 'apply "argument incorrect length")]
	  					  	  [(list? (1st args)) (1st args)]
	  					  	  [else (cons (1st args) (apply-helper (cdr args)))]))])
	  				(apply-proc (1st args) args-ls))]
	  [(map) (let helper ([rest (cdr args)])
	  				(cond [(ormap null? rest)
	  							(if (andmap null? rest)
	  								(list)
	  								(eopl: 'map "arguments have inequal length"))]
	  					  [else (cons (apply-proc (1st args) (map car rest))
	  					  			  (helper (map cdr rest)))]))]
	  [(quotient) (quotient (1st args) (2nd args))]
	  [(void) (void)]
	  [(values) args]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.



(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))









