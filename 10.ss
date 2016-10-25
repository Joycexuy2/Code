;Name: Fangyuan Wang
;Assignment 10

;#1 helper method
(define delete-duplicate
	[lambda (ls)
		(let helper ([ls ls] [newls (list)])
			(if [null?  ls]
				newls
				(if (member (car ls) newls)
					(helper (cdr ls) newls)
					(helper (cdr ls) (append newls (list (car ls)))))))])

;#1-a
(define free-vars
	[lambda (e)
		(delete-duplicate 
			(let helper ([e e] [vars (list)])
				(cond [(null? e) (list)]
					  [(symbol? e)		;variable
					     (if [member e vars]
					     	 (list)
					     	 (list e))]
					   [(eq? (car e) 'lambda)  ;abstraction
					   	 (helper (caddr e) (append vars (cadr e)))]
					   [else 					;application
					   	 (append (helper (car e) vars) (helper (cadr e) vars))])))])

;#1-b
(define bound-vars
	[lambda (e)
		(delete-duplicate
			(let helper ([e e] [vars (list)])
				(cond [(null? e) (list)]
					  [(symbol? e)
					  	(if [member e vars]
					  		(list e)
					  		(list))]
					  [(eq? (car e) 'lambda)
					  	(helper (caddr e) (append vars (cadr e)))]
					  [else
					  	(append (helper (car e) vars) (helper (cadr e) vars))])))])

;#2-a
(define occurs-free?
  [lambda (var exp)
    (cond
      [(symbol? exp) (eqv? var exp)]
      [(null? exp) #f]
      [(eqv? (car exp) 'lambda) 
       (and (not (member var (cadr exp)))
            (occurs-free? var (caddr exp)))]
      [(eqv? (car exp) 'if) 
      	(or (occurs-free? var (cadr exp))
      		(occurs-free? var (caddr exp))
      		(occurs-free? var (cadddr exp)))]
      [(eqv? (car exp) 'set!) 
      	(and #f (or (occurs-free? var (cadr exp))
      		(occurs-free? var (caddr exp))))]
      [(eqv? (car exp) 'let) 
      	(or (occurs-free? var (map cadr (cadr exp)))
      		(and (not (member var (map car (cadr exp))))
      			 (occurs-free? var (caddr exp))))]
      [(eqv? (car exp) 'let*) 
      	(or (let helper ([ls (cadr exp)])
      			(cond [(null? ls) #f]
      				  [(occurs-free? var (cadar ls)) #t]
      				  [(eqv? (caar ls) var) #f]
      				  [else (helper (cdr ls))]))
      		(and (not (member var (map car (cadr exp))))
      			 (occurs-free? var (caddr exp))))]
      [else (or (occurs-free? var  (car exp))
                (occurs-free? var (cdr exp)))])])

;#2-b
(define occurs-bound?
  [lambda (var exp)
    (cond
      [(symbol? exp) #f]
      [(null? exp) #f]
      [(eqv? (car exp) 'lambda)
        (or (occurs-bound? var (caddr exp))
           (and (member var (cadr exp))
                (occurs-free? var (caddr exp))))]
      [(eqv? (car exp) 'if)
      	 (or (occurs-bound? var (cadr exp))
      	 	 (occurs-bound? var (caddr exp))
      	 	 (occurs-bound? var (cadddr exp)))]
      [(eqv? (car exp) 'let)
      	 (or (occurs-bound? var (map cadr (cadr exp)))
      	 	 (or (occurs-bound? var (caddr exp))
      	 	 	 (and (member var (map car (cadr exp)))
      	 	 	 	  (occurs-free? var (caddr exp)))))]
      [(eqv? (car exp) 'let*)
      	 (or (let helper ([ls (cadr exp)])
      	 		(cond [(null? ls) #f]
      	 			  [(occurs-bound? var (cadar ls)) #t]
      	 			  [else (or (and (eqv? var (caar ls)) (occurs-free? var (map cadr ls)))
      	 			  			(helper (cdr ls)))]))
      	 	 (or (occurs-bound? var (caddr exp))
      	 	 	 (and (member var (map car (cadr exp)))
      	 	 	 	  (occurs-free? var (caddr exp)))))]
      [(eqv? (car exp) 'set!)
      	 	(or (occurs-bound? var (cadr exp))
      	 	 	(occurs-bound? var (caddr exp)))]
      [else (or (occurs-bound? var  (car exp))
                (occurs-bound? var (cdr exp)))])])

;#3 helper method
(define list-index
	[lambda (var ls)
		(cond [(null? ls) #f]
			  [(eqv? (car ls) var) 0]
			  [else (+ 1 (list-index var (cdr ls)))])])

;#3 helper method
(define get-bound-list
	[lambda (bound-list newls)
		(let* ([bounded
				 (let bounded ([rest bound-list])
				 	(cond [(null? rest) (list)]
				 		  [(member (caar rest) newls)
				 		  		(cons (list (caar rest) 0 (list-index (caar rest) newls)) 
				 		  			  (bounded (cdr rest)))]
				 		  [else
				 		  	(cons (list (caar rest) (+ 1 (cadar rest)) (caddar rest))
				 		  		  (bounded (cdr rest)))]))]
			   [bounded-var (map car bounded)])
			(let update-new ([rest newls])
				(cond [(null? rest) bounded]
					  [(member (car rest) bounded-var)
					  		(update-new (cdr rest))]
					  [else 
					  	(cons (list (car rest) 0 (list-index (car rest) newls))
					  		  (update-new (cdr rest)))])))])

;#3 helper method
(define convert-exp
	[lambda (sym bound-list)
		(let helper ([rest bound-list])
			(cond [(null? rest) (list ': 'free sym)]
				  [(eqv? sym (caar rest)) (list ': (cadar rest) (caddar rest))]
				  [else (helper (cdr rest))]))])


;#3
(define lexical-address
	[lambda (e)
		(let helper ([e e] [bound-ls (list)])
			(cond [(null? e) (list)]
				  [(symbol? e) (convert-exp e bound-ls)]
				  [(eqv? 'lambda (car e))
				  		(list 'lambda (cadr e) (helper (caddr e) (get-bound-list bound-ls (cadr e))))]
				  [(eqv? 'if (car e))
				  		(list 'if (helper (cadr e) bound-ls) (helper (caddr e) bound-ls) (helper (cadddr e) bound-ls))]
				  [(eqv? 'let (car e))
				  		(list 'let (map (lambda (x) (list (car x) (helper (cadr x) bound-ls))) (cadr e)) (helper (caddr e) (get-bound-list bound-ls (map car (cadr e)))))]
				  [(eqv? 'set! (car e))
				  		(list 'set! (cadr e) (helper (caddr e) bound-ls))]
				  [else (cons (helper (car e) bound-ls) (helper (cdr e) bound-ls))]))])

;#4 helper method
(define convert-back
	[lambda (exp bound-list)
		(let helper ([rest bound-list])
			(cond [(eqv? 'free (cadr exp)) (caddr exp)]
				  [else
				  	(let find-bound ([rest bound-list])
				  		(cond [(null? rest) (list)]
				  			  [(and (eqv? (cadar rest) (cadr exp)) (eqv? (caddar rest) (caddr exp)))
				  			  		(caar rest)]
				  			  [else(find-bound (cdr rest))]))]))])

;#4
(define un-lexical-address
	[lambda (exp)
		(let helper ([exp exp] [bound-list (list)])
			(cond [(null? exp) (list)]
				  [(eqv? ': (car exp)) (convert-back exp bound-list)]
				  [(eqv? 'lambda (car exp))
				  		(list 'lambda (cadr exp) (helper (caddr exp) (get-bound-list bound-list (cadr exp))))]
				  [(eqv? 'if (car exp))
				  		(list 'if (helper (cadr exp) bound-list) (helper (caddr exp) bound-list) (helper (cadddr exp) bound-list))]
				  [(eqv? 'set! (car exp))
				  		(list 'set! (cadr exp) (helper (caddr exp) bound-list))]
				  [(eqv? 'let (car exp))
				  		(list 'let (map (lambda (x) (list (car x) (helper (cadr x) bound-list))) (cadr exp)) (helper (caddr exp) (get-bound-list bound-list (map car (cadr exp)))))]
				  [else (cons (helper (car exp) bound-list) (helper (cdr exp) bound-list))]))])