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
	    (var (lambda (y) (andmap (lambda (x) 
				(or (symbol? x) 
					(pair? x))) y)))
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
    [let*-exp
    	(var (list-of symbol?))
      	(val (list-of expression?))
      	(body (check-eles-in-list expression?))]
	[letrec-exp
		(names (list-of symbol?))
      	(idss (lambda (x) (ormap (lambda (y) (or ((list-of symbol?) y)
												 ((list-of improper?) y))) x)))
      	(bodies (list-of expression?))
		(letrec-body (check-eles-in-list expression?))]
	[named-let-exp
      	(name symbol?)
		(var (list-of symbol?))
      	(val (list-of expression?))
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
	[define-exp
		(var symbol?)
		(body expression?)]
	)

	
	

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
  [empty-env-record]
  [extended-env-record
   (syms (lambda (y) (andmap (lambda (x) 
				(or (symbol? x) 
					(pair? x)
					(improper? x))) y)))
   (vals (list-of scheme-value?))
   (env environment?)]
   [recursively-extended-env-record
		(proc-names (list-of symbol?))
		(idss (lambda (x) (ormap (lambda (y) (or ((list-of symbol?) y)
												 ((list-of improper?) y))) x)))
		(bodies (list-of expression?))
		(env environment?)])

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
   [closure 
	(params (lambda (y) (andmap (lambda (x) 
				(or (symbol? x) 
					(pair? x))) y)))
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
				
(define map-ordered
	(lambda (proc exp)
		(let helper ([e exp])
			(if (null? e)
				(list)
				(cons (proc (car e)) (helper (cdr exp)))))))

(define expand-let
	[lambda (ls newls)
		(if [null? (cdr ls)]
			(list 'let ls newls)
			(list 'let (list (car ls)) (expand-let (cdr ls) newls)))])

(define let*->let
	[lambda (ls)
		(expand-let (cadr ls) (caddr ls))])

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
					;cond
					;[(eq? 'ref (car datum))
					;	(ref-exp (cadr datum))]
					[(eq? 'cond (car datum)) 
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
					[(eq? 'case (car datum)) 
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
												(case-exp (parse-exp test-exps) (map parse-exp comps) list-of-bodies)
											(eopl:error 'parse-exp "Found an else not at the end of a case expression: ~s" datum)))])]
					[(eq? 'begin (car datum)) 
						(cond 
							[(null? (cdr datum))
								(eopl:error 'parse-exp "found an empty begin: ~s" datum)]
							[else 
								(begin-exp (map parse-exp (cdr datum)))])]
					[(eq? 'and (car datum)) (if (null? (cdr datum))
							(and-exp (list (lit-exp #t)))
							(and-exp (map parse-exp (cdr datum))))]
					[(eq? 'or (car datum)) 
						(if (null? (cdr datum))
							(or-exp (list (lit-exp #f)))
							(or-exp (map parse-exp (cdr datum))))]
					[(eq? 'while (car datum)) 
						(cond 
							[(or (null? (cdr datum)) (null? (cddr datum)))
								(eopl:error 'parse-exp "No test/body expressions found in a while expression: ~s" datum)]
							[else
								(while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]
	   	 		 	[(eq? (1st datum) 'lambda);lambda
	   	  				(if (< (length (cdr datum)) 2)
	   	      			(eopl:error 'parse-exp "Error in parse-exp: lambda: incorrect length: " datum)
			  			  	(cond																							
	   	        				[(and (list? (2nd datum))) 
									; ((check-eles-in-set symbol?) (2nd datum)))
									(lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
	   	        				[(symbol? (2nd datum)) 
									(lambda-symbol-exp (2nd datum) (map parse-exp (cddr datum)))]
								[(improper? (2nd datum))
									(lambda-improper-exp (2nd datum) (map parse-exp (cddr datum)))]
	   	        				[(not(list? (2nd datum)))
									(lambda-exp (2nd datum) (map parse-exp (cddr datum)))]
	   	        				[else
					                (eopl:error 'parse-exp "Error in parse-exp: lambda argument list: formals must be symbols: " datum)]))]
									
					[(eq? (1st datum) 'letrec)
						(if (not (> (length datum) 1))
						(eopl:error 'parse-exp "Error in parse-exp: letrec: incorrect length: " datum)
					 	(cond
							[(and (list? (2nd datum))
								((check-eles-in-set symbol?) (map car (2nd datum))))
									(letrec-exp (map car (2nd datum))(map cadadr (2nd datum)) 
										(map parse-exp (map car (map cddadr (2nd datum)))) (map parse-exp (cddr datum)))]
							[else
								(eopl:error 'parse-exp "Error in parse-exp: letrec:" datum)]))]
		   			[(and (eq? (1st datum) 'let);named-let
						(symbol? (2nd datum)))
		         	(if (not (> (length datum) 1))
						(eopl:error 'parse-exp "Error in parse-exp: named-let: incorrect length: " datum)
					 	(cond
							[(and (andmap list? (3rd datum))
								((check-eles-in-set symbol?) (map car (3rd datum))))
									(named-let-exp (2nd datum) 
										(map car (3rd datum))
										(map parse-exp (map cadr (3rd datum)))
										(map parse-exp (cdddr datum)))]
							[else
								(eopl:error 'parse-exp "Error in parse-exp: named-let:" datum)]))]
					
						[(or (and (eq? (1st datum) 'let) (list? (2nd datum))));let
							(if (not (> (length datum) 2))
								(eopl:error 'parse-exp "Error in parse-exp: let/let*/letrec: incorrect length: " datum)
		   	  					(cond
									[(and (list? (2nd datum))
										(2-lists? (2nd datum))
										((check-eles-in-set symbol?) (map car (2nd datum))))
										(let-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) 
										(map parse-exp (cddr datum)))]
									[else
										(eopl:error 'parse-exp "Error in parse-exp: let argument list: formals must be symbols:" datum)]))]
						[(eq? (1st datum) 'let*) ;let*
							(if (not (> (length datum) 2))
								(eopl:error 'parse-exp "Error in parse-exp: let/let*/letrec: incorrect length: " datum)
		   	  					(cond
									[(and (list? (2nd datum))
										(2-lists? (2nd datum))
										((check-eles-in-set symbol?) (map car (2nd datum))))
										(let*-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) 
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
			[(eq? (1st datum) 'define)
				(cond 
					[(> (length datum) 3)
						(eopl:error 'parse-exp "Error in parse-exp: define!: Too many parts: " datum)]
					[(< (length datum) 2)
						(eopl:error 'parse-exp "Error in parse-exp: define!: missing parts: " datum)]
					[(and (eq? (length datum) 3) 
			  			(symbol? (2nd datum)))
							(define-exp (2nd datum) (parse-exp (3rd datum)))]
					[else 
						(eopl:error 'parse-exp "Error in parse-exp: define!: " datum)])]
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
    (extended-env-record syms (map (lambda (x) (if (box? x) x (box x))) vals) env)))
	
(define extend-env-recursively
	(lambda (proc-names idss bodies old-env)
		(recursively-extended-env-record
			proc-names idss bodies old-env)))

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
		 
;(define-datatype cell cell?
;	(lambda (v) (list v 'a-cell)))
;	
;(define cell-ref car)
;(define cell-set! set-car!)
(define cell? box?)
(define deref unbox)
(define set-ref! set-box!)

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (deref (apply-env-ref env sym succeed fail))))

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record ()
        (fail)]
      [extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env-ref env sym succeed fail)))]
	  [recursively-extended-env-record
			(procnames idss bodies old-env)
			(let ([pos (list-find-position sym procnames)])
				(if (number? pos)
					(cond [((list-of symbol?)(list-ref idss pos))
						(closure (list-ref idss pos)
							(list(list-ref bodies pos))
							env)]
						[else (closure-imp (list-ref idss pos) (list (list-ref bodies pos)) env)])
					(apply-env-ref old-env sym succeed fail)))])))






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
 				(app-exp (lambda-exp var (map syntax-expand body)) (map syntax-expand val))]

 			[let*-exp (var val body)
 				(syntax-expand 
 					(let helper ([var var] [val val])
 						(cond [(and (null? (cdr var)) (null? (cdr val)))
 								(let-exp var (list (syntax-expand (car val))) (map syntax-expand body))]
 							  [else (let-exp (list (car var)) 
 							  				 (list (syntax-expand (car val))) 
 							  				 (list (helper (cdr var) (cdr val))))])))]	

			[letrec-exp (names idss bodies letrec-body)
				(letrec-exp names idss (map syntax-expand bodies) (map syntax-expand letrec-body))]

 			[named-let-exp (name var val body)
				(letrec-exp (list name) (list var) (map syntax-expand body) (list (app-exp (var-exp name) val)))]
												;previously we just passed"body", but it should be syntax expanded
												;before we passed it to the letrec-exp. That was my fault. I already submited
												;to the server.

 			[set!-exp (var body)
 				(set!-exp var (syntax-expand body))]
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
						[(eq? (car tes) '(else)) (syntax-expand (car val))] 
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
				(let help ([tes tests][comp comparators][val bodies])
					(cond 
						[(null? comp) (app-exp (lit-exp '(void)))]
						[(eq? (car comp) '(else)) (syntax-expand (car val))] 
						[else(app-exp
							(lambda-exp 
								(list 'x)
								(list 
									(if-without-else-exp 
										(lit-exp (and (list? (member (var-exp 'x) (syntax-expand (car comp))))))
										(syntax-expand (car val))
										(help tes (cdr comp) (cdr val))))) 
							(list (syntax-expand tes)))]))]
			[while-exp (test bodies)
				(while-exp (syntax-expand test) (map syntax-expand bodies))]
			[begin-exp (bodies)
				(app-exp (lambda-exp '() (map syntax-expand bodies)) (list))]
			[define-exp (var body)
				(define-exp var (syntax-expand body))]
			;[ref-exp (id) 
			;	(ref-exp (box id))]
			[else exp])))






;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+


(define *prim-proc-names* '(+ - * / not add1 sub1 zero? not < > >= <= = cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr list null? assq eq? equal? 
atom? length list->vector list? pair? procedure? vector->list vector
make-vector vector-ref vector? number? symbol? set-car!  set-cdr!
vector-set! display newline quote apply map quotient eqv? list-tail append))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env))) 

(define global-env init-env)

(define reset-global-env
	(lambda () (set! global-env init-env)))

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

(define get-refs
  (lambda (rands env)
    (map 
      (lambda (x)
        (if (eqv? 'var-exp (1st x))
          (apply-env-ref env (2nd x)
            (lambda (x) x)
            (lambda () 
              (apply-env-ref global-env (2nd x)
                (lambda (x) x)
                (lambda ()
                  (eopl:error 'get-refs
                    "variable not found in environment: ~s" (2nd x))))))
          (void)))
      rands)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (let ([identity-proc (lambda (x) x)])
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
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
		(apply-env-ref env id ; look up its value.
      	   (lambda (x) (deref x)) ; procedure to call if id is in the environment 
           (lambda () 
				(apply-env-ref global-env
				id
				(lambda (x) (deref x))
				(lambda ()
					(eopl:error 'apply-env ; procedure to call if id not in env
						"variable not found in environment: ~s"
						id)))))] 
	  ;add or and case cond
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands-vars rands env)]
              [refs (get-refs rands env)])
          (apply-proc proc-value args refs))]
	  [letrec-exp (name idss bodies letrec-body)
		(eval-bodies letrec-body (extend-env-recursively name idss bodies env))]
	  [set!-exp (var body)
		(let ([ref (apply-env-ref env var (lambda (x) x) (lambda () 
								(apply-env-ref init-env var (lambda (x) x) (lambda () '#f))))])
				(if ref
					(set-ref! ref (eval-exp body env))
					(set! global-env (extend-env (list var) (list (eval-exp body env)) global-env))))]
	  [define-exp (var body)
		(set! global-env (extend-env (list var) (list (eval-exp body env)) global-env))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

; evaluate the list of operands, putting results into a list

(define eval-rands
 (lambda (rands env)
  (map (lambda (x) (eval-exp x env)) rands)))
	
(define eval-rands-vars
  (lambda (rands env)
    (map (lambda (exp) (cases expression exp
			[var-exp (var) (apply-env-ref env var (lambda (x) (deref x)) 
												  (lambda () 
														(apply-env-ref global-env var (lambda (x) (deref x))
																				  (lambda ()
																				  (eopl:error 'apply-env "Reference not found")))))]
			[else 
				(eval-exp exp env)])) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args refs)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
	  ;closure
	  [closure (params bodies env)
	  	(let* ([id-ls (map (lambda (x) (if (list? x)
	  									   (2nd x)
	  									   x))
	  					   params)]
	  		   [new-env (extend-env id-ls args env)] 
	  		   [result (eval-bodies bodies new-env)])
	  		   (let helper ([rest params] [index 0])
	  		   		(cond [(null? rest) (void)]
	  		   			  [(list? (car rest))
	  		   			  		(set-ref! (list-ref refs index)
	  		   			  				  (apply-env-ref new-env (2nd (car rest))
                                        	(lambda (x) (deref x))
                                        	(lambda () 
                                          		(eopl:error 'apply-proc
                                            	"variable not found in environment: ~s" (2nd (car rest))))))
	  		   			  		(helper (cdr rest) (+ 1 index))]
	  		   			  [else (helper (cdr rest) (+ 1 index))]))
	  		    result)]

	  [closure-imp (params bodies env)
	  	(let* ( [id-ls (map (lambda (x) (if (list? x)
	  									   (cadr x)
	  									   x))
	  					   params)]
	  			[new-env ((package-imp-args params (total-imp-params params 1)) (package-imp-args args (total-imp-params params 1)) env)]
	  			[result (eval-bodies bodies new-env)])
	  		   (let helper ([rest params] [index 0])
	  		   		(cond [(null? rest) (void)]
	  		   			  [(list? (car rest))
	  		   			  		(set-ref! (list-ref refs index)
	  		   			  				  ((apply-env-ref new-env (2nd (car rest))
                                        	(lambda (x) (deref x))
                                        	(lambda () 
                                          		(eopl:error 'apply-proc
                                            	"variable not found in environment: ~s" (2nd (car rest)))))))
	  		   			  		(helper (cdr rest) (+ 1 index))]
	  		   			  [else (helper (cdr rest) (+ 1 index))]))
	  		   result)]
	  [closure-sym (params bodies env)
		(eval-bodies bodies
					(extend-env (list params) (list args) env))]
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define total-imp-params
	(lambda (ls acc)
		(if (or (null? (cdr ls))(symbol? (cdr ls)))
			acc
			(total-imp-params (cdr ls) (+ acc 1)))))
			
					
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
	  [(apply) (let ([args-ls 
	  					(let apply-helper ([args (cdr args)])
	  						(cond [(null? args) (eopl: 'apply "argument incorrect length")]
	  					  	  [(list? (1st args)) (1st args)]
	  					  	  [else (cons (1st args) (apply-helper (cdr args)))]))])
	  				(apply-proc (1st args) args-ls (list)))]
	  [(map) (let helper ([rest (cdr args)])
	  				(cond [(ormap null? rest)
	  							(if (andmap null? rest)
	  								(list)
	  								(eopl: 'map "arguments have inequal length"))]
	  					  [else (cons (apply-proc (1st args) (map car rest) (list))
	  					  			  (helper (map cdr rest)))]))]
	  [(quotient) (quotient (1st args) (2nd args))]
	  [(eqv?) (eqv? (1st args) (2nd args))]
	  [(list-tail) (list-tail (1st args) (2nd args))]
	  [(append) (append (1st args) (2nd args))]
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









