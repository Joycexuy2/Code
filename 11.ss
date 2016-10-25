;Name: Fangyuan Wang
;Assignment 11

;#1-a
(define-syntax my-let
	(syntax-rules ()
		[(_ ((x v) ...) e1 e2 ...)
		 ((lambda (x ...) e1 e2 ...)  v ...)]
		[(_ n ((x v) ...) exp) 
			(letrec ([n (lambda (x ...) exp)])
				(n v ...))]))

;#1-b
(define-syntax my-or
	(syntax-rules ()
		[(_) #f]
		[(_ e1) e1]
		[(_ e1 e2 ...)
			(let ([x e1])
				(if x x (my-or e2 ...)))]))

;#1-c
(define-syntax +=
	(syntax-rules ()
		[(_ x v) (begin (set! x (+ v x))
						x)]))

;#1-d
(define-syntax return-first
	(syntax-rules ()
		[(_ e1) e1]
		[(_ e1 e2 ...)
			(let ([return-val e1])
				(begin e2 ... return-val))]))

;; Binary trees using define-datatype
(load "chez-init.ss") ; chez-init.ss should be in the same folder as this code.
;; from EoPL, page 50
(define-datatype bintree bintree?
 (leaf-node
 (num integer?))
 (interior-node
 (key symbol?)
 (left-tree bintree?)
 (right-tree bintree?)))

;#2
(define bintree-to-list
	[lambda (bt)
		bt])

;#3 helper method
(define leaf-sum
	[lambda (tree)
		(cases bintree tree
			[leaf-node (datum) datum]
			[interior-node (key left-tree right-tree)
				(+ (leaf-sum left-tree) (leaf-sum right-tree))])])

;#3 helper method
(define isleaf?
	[lambda (tree)
		(cases bintree tree
			[leaf-node (datum) #t]
			[interior-node (key left-tree right-tree) #f])])

;#3 helper method
(define max-interior-helper
	[lambda (T)
		(cases bintree T
			[leaf-node (datum) #f]
			[interior-node (key left-tree right-tree)
				(cond [(and (isleaf? left-tree) (isleaf? right-tree))
						 (let ([sum (leaf-sum T)])
							(cons (cons key sum) sum))]
				  	  [(and (not (isleaf? left-tree)) (isleaf? right-tree))
				  		 (let* ([left-max (max-interior-helper left-tree)]
				  				  [left-max-sum (cdar left-max)] 
				  				  [left-sum (cdr left-max)]
				  				  [total-sum (+ left-sum (leaf-sum right-tree))])
				  			(if (< left-max-sum total-sum)
				  				(cons (cons key total-sum) total-sum)
				  				(cons (car left-max) total-sum)))]
				  	  [(and (not (isleaf? right-tree)) (isleaf? left-tree))
				  		  (let* ([right-max (max-interior-helper right-tree)]
				  			     [right-max-sum (cdar right-max)]
				  				 [right-sum (cdr right-max)]
				  				 [total-sum (+ right-sum (leaf-sum left-tree))])
				  					(if (< right-max-sum total-sum)
				  						(cons (cons key total-sum) total-sum)
				  						(cons (car right-max) total-sum)))]
				  	  [else 
				  	  	   (let* ([right-max (max-interior-helper right-tree)] 
				  				  [left-max (max-interior-helper left-tree)]
				  				  [right-max-sum (cdar right-max)] 
				  				  [right-sum (cdr right-max)] 
				  				  [left-max-sum (cdar left-max)] 
				  				  [left-sum (cdr left-max)]
				  				  [total-sum (+ left-sum right-sum)])
				  					(if [< left-max-sum right-max-sum]
				  						(if [< right-max-sum total-sum]
				  							(cons (cons key total-sum) total-sum)
				  							(cons (car right-max) total-sum))
				  						(if [< left-max-sum total-sum]
				  							(cons (cons key total-sum) total-sum)
				  							(cons (car left-max) total-sum))))])])])


;#3
(define max-interior
	[lambda (T)
		(caar (max-interior-helper T))])

(load "chez-init.ss"); This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.

;#4 helper method
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define lit?
	(lambda (e)
		(or [number? e] [string? e] [symbol? e] [boolean? e]
			[char? e] [vector? e] [null? e] [and (list? e) (list? (cdr e)) (eqv? 'quote (car e))])))

(define list-of-exp?
	[lambda (e)
		(if (list? e)
			(andmap expression? e)
			#f)])

(define lambda-var?
	[lambda (e)
		(cond [(symbol? e) #t]
			  [(null? e) #t]
			  [(pair? e)
			  		(let helper ([rest e])
			  			(cond [(null? rest) #t]
			  			  	  [(symbol? rest) #t]
			  			      [else (and (symbol? (car rest)) (helper (cdr rest)))]))]
			  [else #f])])

(define-datatype expression expression?
	[var-exp (id symbol?)]
	[lit-exp (id lit?)]
	[lambda-exp (var expression?) (body list-of-exp?)]
	[lambda-var-exp (var lambda-var?)]
	[set!-exp (id symbol?) (r-val-exp expression)]
	[if-else-exp (condition-exp expression?) (true-exp expression?) (false-exp expression?)]
	[if-exp (condition-exp expression?) (true-exp expression?)]
	[let-exp (ls list-of-exp?) (body list-of-exp?)]
	[single-let-exp (var expression?) (body expression?)]
	[named-let-exp (name expression?) (ls list-of-exp?) (body list-of-exp?)]
	[let*-exp (ls list-of-exp?) (body list-of-exp?)]
	[letrec-exp (ls list-of-exp?) (body list-of-exp?)]
	[app-exp (rator expression?) (rand list-of-exp?)])

;#4-a
(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(lit? datum) (lit-exp datum)]
     ;set! expression
     [(eqv? (1st datum) 'set!)
     	(if (= 3 (length datum))
     		(if (symbol? (2nd datum))
     			(set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
     			(eopl:error 'parse-exp "declaration in set-expression ~s must take a symbol" (2nd datum)))
     		(eopl:error 'parse-exp "set-expression ~s has incorrect arguments" datum))]
     [(pair? datum)
      (cond 
      		;improper list
      		[(not (list? datum)) (eopl:error 'parse-exp "expression ~s is an improper list " datum)]
       		;lambda expression
       		[(eqv? 'lambda (1st datum))
       			(cond [(< 2 (length datum))
       					(if (lambda-var? (2nd datum))
       						(lambda-exp (lambda-var-exp (2nd datum)) (map parse-exp (cddr datum)))
       						(eopl:error 'parse-exp "lambda's formal argument ~s must be symbols " (2nd datum)))]
       				  [else (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)])]
       		;if expression
       		[(eqv? 'if (1st datum))
       			(cond [(= 3 (length datum))
       						(if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
       				  [(= 4 (length datum))
       				  		(if-else-exp (parse-exp (2nd datum)) 
       				  			 		 (parse-exp (3rd datum)) 
       				  			 		 (parse-exp (cadddr datum)))]
       				  [else (eopl:error 'parse-exp "if-expression: ~s does not have the right format: condition, then, else" datum)])]
       		;let-type expression
       		[(or (eqv? 'let (1st datum)) (eqv? 'letrec (1st datum)) (eqv? 'let* (1st datum)))
       			(cond [(> 3 (length datum))
       					(eopl:error 'parse-exp "let-type-expression has incorrect length")]
       				  [else
       				  	 (letrec ([parse-let
       				  	 			(lambda (ls)
       				  	 				(let helper ([rest ls])
       				  	 					(cond [(null? rest) (list)]
       				  	 						  [(not (list? rest))
       				  	 						  	(eopl:error 'parse-exp "~s-list is not a proper list ~s" (1st datum) rest)]
       				  	 						  [(not (list? (car rest)))
       				  	 						  	(eopl:error 'parse-exp "declaration in ~s-list is not a proper list" (1st datum) (car rest))]
       				  	 						  [(not (= 2 (length (car rest))))
       				  	 						  	(eopl:error 'parse-exp "declaration in ~s-list must be in length of two ~s" (1st datum) (car rest))]
       				  	 						  [(not (symbol? (caar rest)))
       				  	 						  	(eopl:error 'parse-exp "variable in ~s must be a symbol ~s" (1st datum) (caar rest))]
       				  	 						  [else 
       				  	 						  		(cons (single-let-exp (parse-exp (caar rest))
       				  	 						  							  (parse-exp (cadar rest)))
       				  	 						  			  (helper (cdr rest)))])))])
       				  	 	   (cond [(symbol? (2nd datum))
       				  	 	   			(cond [(not (eqv? 'let (1st datum)))
       				  	 	   						(eopl:error 'parse-exp "declaration in ~s must be a proper list ~s" (1st datum) (2nd datum))]
       				  	 	   				  [(= 3 (length datum))
       				  	 	   				  		(eopl:error 'parse-exp "named let expression has incorrect length ~s" datum)]
       				  	 	   				  [else (named-let-exp (var-exp (2nd datum))
       				  	 	   				  					   (parse-let (3rd datum))
       				  	 	   				  					   (map parse-exp (cdddr datum)))])]
       				  	 	   		 [(eqv? 'let (1st datum))
       				  	 	   		 	(let-exp (parse-let (2nd datum)) (map parse-exp (cddr datum)))]
       				  	 	   		 [(eqv? 'let* (1st datum))
       				  	 	   		 	(let*-exp (parse-let (2nd datum)) (map parse-exp (cddr datum)))]
       				  	 	   		 [else 
       				  	 	   		 	(letrec-exp (parse-let (2nd datum)) (map parse-exp (cddr datum)))]))])]
       		;application
       		[else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))])]
     ;error
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

; ;#4-b
(define unparse-exp
	[lambda (e)
		(cases expression e
			[var-exp (id) id]
			[lit-exp (id) id]
			[lambda-exp (var body)
				(cons 'lambda (cons (unparse-exp var) (map unparse-exp body)))]
			[lambda-var-exp (var) var]
			[set!-exp (id r-val-exp)
				(list 'set! (unparse-exp id) (unparse-exp r-val-exp))]
			[if-else-exp (condition-exp true-exp false-exp)
				(list 'if (unparse-exp condition-exp) (unparse-exp true-exp) (unparse-exp false-exp))]
			[if-exp (condition-exp true-exp)
				(list 'if (unparse-exp condition-exp) (unparse-exp true-exp))]
			[let-exp (ls body)
				(cons 'let (cons (map unparse-exp ls) (map unparse-exp body)))]
			[single-let-exp (var body)
				(list (unparse-exp var) (unparse-exp body))]
			[named-let-exp (name ls body)
				(cons 'let (cons (unparse-exp name) (cons (map unparse-exp ls) (map unparse-exp body))))]
			[let*-exp (ls body)
				(cons 'let* (cons (map unparse-exp ls) (map unparse-exp body)))]
			[letrec-exp (ls body)
				(cons 'letrec (cons (map unparse-exp ls) (map unparse-exp body)))]
			[app-exp (rator rand)
				(cons (unparse-exp rator) (map unparse-exp rand))])])