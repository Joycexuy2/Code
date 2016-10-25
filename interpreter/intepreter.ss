;;673-676
; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env)
    (cases expression exp
	  [keys-list-exp (datum) datum]
      [lit-exp (datum) datum]
      [var-exp (id) (apply-env env id ;look up its value.
      	(lambda (x) x) ;procedure to call if id is in the environment 
        (lambda () (eopl:error 'apply-env ;called if id not in env
		   "variable not found in environment: ~s" id)))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
	  [quote-exp (datum) datum]
	  [if-exp (test-exp then-exp else-exp)
		(if (eval-exp test-exp env)
			(eval-exp then-exp env)
			(eval-exp else-exp env))]
	  [letrec-exp (proc-names idss bodies letrec-body)
		(cond [(and (pair? idss)(list? idss)) (eval-bodies letrec-body (extend-env-recursively
								(unparse-exp-list proc-names) idss bodies env))]
				[else (eopl:error 'eval-exp "Bad idss field: ~a" idss)])]
	  [letrec-exp-improper (proc-names improper proper letrec-body)
		(eval-bodies letrec-body (extend-env-recursively-improper
								(unparse-exp-list proc-names) improper proper env))]
	  [let-exp (vars exps bodies)
		(let ([new-env (extend-env (unparse-exp-list vars) (eval-rands exps env) env)])
			(eval-bodies bodies new-env))]
	  [while-exp (test-exp then-exp) (letrec ([loop (lambda ()
	   (if (eval-exp test-exp env) (begin (eval-exp (syntax-expand then-exp) env) (loop))
	   (lit-exp (list (void)))))]) (loop))]
	  [lambda-exp (vars bodies) (cond
								[(expression? vars) (closure (unparse-exp vars) bodies env)]
								[((list-of expression?) vars)(closure (unparse-exp-list vars) bodies env)]
								[else (display vars)]
										)]
	  [lambda-exp-improper (vars improper-var bodies) 
	   (closure-improper (unparse-exp-list vars) (unparse-exp improper-var) bodies env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))
	  
; evaluate the list of operands, putting results into a list
(define eval-bodies (lambda (bodies env) 
	(let loop ([bodies bodies])
		(if (null? (cdr bodies))
			(eval-exp (car bodies) env)
			(begin 
				(eval-exp (car bodies) env)
				(loop (cdr bodies)))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define make-improper (lambda (args len index)
	(cond [(<= index len) (cons (car args) 
							(make-improper (cdr args) len (+ index 1)))]
		  [else (list args)])))
	
(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
	  [closure (vars bodies env) (cond [((list-of symbol?)vars)(eval-bodies bodies (extend-env vars args env))]
										[else (eval-bodies bodies (extend-env (list vars) (list args) env))])]
	  [closure-improper (vars improper-var bodies env) 
	   (eval-bodies bodies (extend-env (append vars (list improper-var)) 
	      (make-improper args (length vars) 1) env))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / sub1 add1 cons = < <= >= > not zero? 
  car cdr list list? null? eq? equal? eqv? length pair? atom? list-vector 
  vector-list procedure? vector vector-list vector? make-vector vector-ref number? 
  symbol? set-car! set-cdr! vector-set! display newline caaar caadr caar cadar
  caddr cadr cdaar cdadr cddar cdddr cddr list->vector vector->list vector 
  vector-list vector? make-vector vector-ref number? symbol? set-car! set-cdr! 
  vector-set! display newline apply map quotient memv append list-tail))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args) (cond [(null? prim-proc) (error 'apply-prim-proc
    "You have no primitive procedure specified")]
	[(null? args) (error 'apply-prim-proc "You have no arguments specified")]
    [else (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
	  [(add1) (if (= 1 (length args)) (+ 1 (1st args)) (error 'apply-prim-proc
		"You need to pass one argument into add1, args: " args))]
      [(sub1) (if (= 1 (length args)) (- (1st args) 1) (error 'apply-prim-proc
		"You need to pass one argument into sub1, args: " args))]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(>) (apply > args)]
      [(not) (not (1st args))]
      [(zero?) (= 0 (1st args))]
      [(car) (car (1st args))] [(cadr) (car (cdr (1st args)))]
	  [(caddr) (car (cdr (cdr (1st args))))] [(cdr) (cdr (1st args))]
	  [(cddr) (cdr (cdr (1st args)))] [(cdddr) (cdr (cdr (cdr (1st args))))]
	  [(caar) (car (car (1st args)))] [(caaar) (car (car (car (1st args))))]
	  [(caadr) (car (car (cdr args)))] [(cadar) (car (cdr (car (1st args))))]
	  [(cdaar) (cdr (car (car (1st args))))] [(cdadr) (cdr (car (cdr (1st args))))]
	  [(cddar) (cdr (cdr (car (1st args))))]
      [(list) (apply list args)]
	  [(list?) (apply list? args)]
	  [(quotient) (quotient (1st args) (2nd args))]
      [(null?) (apply null? args)]
      [(eq?) (if (null? (cdr args)) (error 'apply-prim-proc "eq? requires 2 args")
		(eq? (1st args) (2nd args)))]
      [(equal?) (if (null? (cdr args)) (error 'apply-prim-proc "eq? requires 2 args")
		(equal? (1st args) (2nd args)))]
      [(eqv?) (if (null? (cdr args)) (error 'apply-prim-proc "eq? requires 2 args")
		(eqv? (1st args) (2nd args)))]
		[(length) (apply length args)]
		[(pair?) (pair? args)]
		[(atom?) (not (pair? args))]
		[(procedure?) (apply proc-val? args)]
		[(list->vector) (apply list->vector args)]
		[(vector->list) (apply vector->list args)]
		[(vector) (apply vector args)]
		[(vector-list) (vector-list args)]
		[(vector?) (apply vector? args)]
		[(make-vector) (if (number? (1st args)) (if (null? (cdr args))
		 (make-vector (1st args)) (make-vector (1st args) (2nd args)))
		 (error 'apply-prim-proc "First argument to make-vector must be a number"))]
		[(vector-ref) (vector-ref (1st args) (2nd args))]
		[(number?) (if (= 1 (length args)) (number? (1st args)) (error 
		 'apply-prim-proc "number? can only be applied to an arg of length 1, not arg: " arg))]
		[(symbol?) (if (= 1 (length args)) (symbol? (1st args)) (error
		 'apply-prim-proc "symbol? can only be applied to an arg of length 1, not arg: " arg))]
		[(set-car!) (set-car! (1st args) (2nd args))]
		[(set-cdr!) (set-cdr! (1st args) (2nd args))]
		[(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
		[(display) (display args)]
		[(newline) (newline)]
		[(apply) (apply-proc (car args) (cadr args))]
		[(map) (map (lambda (x) (apply-proc (car args) x)) (map list (cadr args)))]
		[(memv) (memv (1st args) (2nd args))]
		[(append) (append (1st args)(2nd args))]
		[(list-tail) (list-tail (1st args)(2nd args))]
	  [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])])))

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
 ;------------------------------------------------------------------
(define begin-loop (lambda (exps)
	(let loop ([exps exps])
		(if (null? (cdr exps))
			(app-exp (lambda-exp (list) (list (car exps))) (list))
			(app-exp (lambda-exp (list (var-exp '_)) (list (car exps))) 
			(list (loop (cdr exps)))))))) 

(define cond-expand (lambda (exps) (let loop ([exps exps])
	(if (null? (cdr exps))
		(cadr (car exps))
		(if-exp (syntax-expand (car (car exps))) 
				(syntax-expand(cadr (car exps))) 
				(syntax-expand(loop (cdr exps))))))))

(define case-expand (lambda (eva cases) (let loop ([cases cases])
	(if (null? (cdr cases))
		(cadr (car cases))
		(if-exp (app-exp (parse-exp 'memv) (list (syntax-expand eva) (car (car cases)))) 
				(syntax-expand (cadr (car cases))) 
				(syntax-expand(loop (cdr cases))))))))
(define and-expand (lambda (exps)(let loop ([exps exps])
	(if (null? exps) (parse-exp #t) (if (null? (cdr exps))
		(car exps)
		(if-exp (syntax-expand (car exps))
				(loop (cdr exps))
				(lit-exp #f))))
	))
)
	
(define or-expand (lambda (exps)(let loop ([exps exps])
	(if (null? exps) (parse-exp #f) (if (null? (cdr exps))
		(car exps)
		(if-exp (syntax-expand (car exps))
				(syntax-expand (car exps))
				(loop (cdr exps)))))
	))
)
				
(define let*-expand (lambda (vars exps bodies)
	(let loop ([vars vars] [exps exps])
		(if (or (null? (cdr exps))(null? (cdr vars)))
			(let-exp (list(car vars))(list (car exps)) bodies)
			(let-exp (list(car vars))(list (car exps)) (list (loop (cdr vars) (cdr exps))))))))	

(define improper-expand (lambda (idss bodies)
	(let loop ([lst idss][blst bodies][resultID '()][resultBody '()])
		(if (or (null? lst) (null? blst))
			(list resultID resultBody)
			(if (and (not (list? (1st lst)))(pair? (1st lst)))
				(loop (cdr lst)(cdr blst) (cons (1st lst) resultID) (cons (1st blst) resultBody))
				(loop (cdr lst)(cdr blst) resultID resultBody))
		))))
	
(define proper-expand (lambda (idss bodies)
	(let loop ([lst idss][blst bodies][resultID '()][resultBody '()])
		(if (or (null? lst) (null? blst))
			(list resultID resultBody)
			(if (and (list? (1st lst))(pair? (1st lst)))
				(loop (cdr lst)(cdr blst) (cons (1st lst) resultID) (cons (1st blst) resultBody))
				(loop (cdr lst)(cdr blst) resultID resultBody)))
		)))
			
(define syntax-expand (lambda (e)
	(cases expression e
		[let-exp (vars exps bodies) 
		 (app-exp (lambda-exp vars (map syntax-expand bodies)) exps)]
		[letrec-exp (proc-names idss bodies letrec-body)
					(cond [((list-of (list-of symbol?)) idss)
							(list 'letrec-exp proc-names idss (map syntax-expand bodies)(map syntax-expand letrec-body))]
						[else (list 'letrec-exp-improper proc-names idss (map syntax-expand bodies)(map syntax-expand letrec-body))])]
;										;proc-names 
;										;(reverse (improper-expand idss (map syntax-expand bodies)))
;										;(reverse(proper-expand idss (map syntax-expand bodies)))
;										;(map syntax-expand letrec-body))])
					
		[begin-exp (exps) (begin-loop (reverse exps))]
		[if-exp (test-exp then-exp else-exp)
		 (if-exp (syntax-expand test-exp) (syntax-expand then-exp) 
		 (syntax-expand else-exp))]
		[cond-exp (exps) (cond-expand exps)]
		[let*-exp (vars exps bodies) (let*-expand vars exps bodies)]
		[case-exp (eva cases) (case-expand eva cases)]
		[and-exp (exps) (and-expand exps)]
		[or-exp (exps) (or-expand exps)]
		[else e])))




