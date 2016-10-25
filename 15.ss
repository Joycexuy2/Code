;Assginment 15
;Name: Fangyuan Wang

;#2 
;Answer:
;The reason why the solution in the slide for fib takes less time is that 
;it takes more time to grow the hashtable. Growing the hashtable needs to rehash
;all the elements in the old hashtable. Also, the solution in the slide records 
;all the results that were calculated for all the numbers that are smaller than the one called.


(define apply-continuation
	[lambda (k . v)
		(apply k v)])

;1-a
(define member?-cps
	[lambda (item ls k)
		(cond [(null? ls) (apply-continuation k #f)]
			  [(eq? (car ls) item)
			  		(apply-continuation k #t)]
			  [else (member?-cps item (cdr ls) k)])])

;1-b
(define set?-cps
	[lambda (ls k)
		(cond [(null? ls) (apply-continuation k #t)]
			  [(not (pair? ls)) (apply-continuation k #f)]
			  [else (set?-cps (cdr ls)
			  			  (lambda (cdr-ls)
			  			  	(member?-cps (car ls) 
			  			  		         (cdr ls)
			  			  		         (lambda (is-member?)
			  			  		         	(apply-continuation k 
			  			  		         		(if is-member?
			  			  		         		#f
			  			  		         		cdr-ls))))))])])

;1-c
(define 1st-cps
	[lambda (ls k)
		(apply-continuation k (car ls))])

(define set-of-cps
	[lambda (ls k)
		(cond [(null? ls) (apply-continuation k '())]
			  [else (set-of-cps (cdr ls)
			  					(lambda (cdr-ls)
			  						(member?-cps (car ls) 
			  									 (cdr ls)
			  									 (lambda (is-member?)
			  									 	(apply-continuation k
			  									 		(if is-member?
			  									 			cdr-ls
			  									 			(cons (car ls) cdr-ls)))))))])])

(define map-cps
	[lambda (proc-cps ls k)
		(cond [(null? ls) (apply-continuation k '())]
			  [else (map-cps proc-cps 
			  				 (cdr ls)
			  				 (lambda (cdr-ls)
			  				 	(proc-cps (car ls)
			  				 			  (lambda (proc-result)
			  				 			  		(apply-continuation k (cons proc-result cdr-ls))))))])])

(define domain-cps
	[lambda (rel k)
		(map-cps 1st-cps rel (lambda (result)
									(set-of-cps result k)))])

;1-d
(define make-cps
	[lambda (proc)
		(lambda (args k)
			(apply-continuation k (proc args)))])

;1-e
(define andmap-cps
	[lambda (pred-cps ls k)
		(cond [(null? ls) (apply-continuation k #t)]
			  [else (pred-cps (car ls)
			  				  (lambda (proc-result)
			  				  	  (if proc-result
			  				  	  	  (andmap-cps pred-cps (cdr ls) (lambda (cdr-ls) (apply-continuation k cdr-ls)))
			  				  	  	  (apply-continuation k #f))))])])

;1-f
(define cps-snlist-recur
	[lambda (base item-proc-cps list-proc-cps)
		(letrec 
			([helper (lambda (ls k)
						(if (null? ls)
							(apply-continuation k base)
							(let ([c (car ls)])
								(if (or (pair? c) (null? c))
									(helper c 
											(lambda (car-result)
												(helper (cdr ls)
													(lambda (cdr-result)
														(list-proc-cps car-result cdr-result k)))))
									(helper (cdr ls)
										    (lambda (cdr-result)
										    	(item-proc-cps c cdr-result k)))))))])
			helper)])

(define +-cps
	[lambda (a b k)
		(apply-continuation k (+ a b))])

(define append-cps
	[lambda (ls1 ls2 k)
		(cond [(null? ls2) (apply-continuation k ls1)]
			  [else (append-cps 
			  			(reverse (cons (car ls2) (reverse ls1)))
			  			(cdr ls2)
			  			k)])])

(define sn-list-reverse-cps
	[cps-snlist-recur (list)
		(lambda (x y k) (append-cps y (list x) k))
		(lambda (x y k) (append-cps y (list x) k))])

(define sn-list-occur-cps
	[lambda (s sls k)
		((cps-snlist-recur 0 (lambda (x y k)
									(if (eq? s x)
										(+-cps 1 y k)
										(apply-continuation k y)))
									+-cps)
		  sls k)])

(define max-cps
	[lambda (x y k)
		(apply-continuation k (max x y))])

(define sn-list-depth-cps
	(cps-snlist-recur 1
		(lambda (x y k) (apply-continuation k y))
		(lambda (x y k) (max-cps (+ 1 x) y k))))

;#2
(define memoize
	[lambda (func hash-proc eq-proc)
		(let ([hashtable (make-hashtable hash-proc eq-proc 16)])
			(lambda args
				(let ([val (hashtable-ref hashtable args #f)])
					(if val
						val
						(let ([result (apply func args)])
							(hashtable-set! hashtable args result)
							result)))))])

;#3
(define-syntax with-values
	(syntax-rules ()
		[(_ exp consumer)
			(call-with-values (lambda () exp) consumer)]))

(define-syntax mv-let
	(syntax-rules ()
		((_ ((x ...) e0) e1 e2 ...)
			(with-values e0
				(lambda (x ...) e1 e2 ...)))))

(define subst-leftmost
	[lambda (new old slist proc)
		(with-values (let helper ([slist slist] [newls (list)])
						(cond [(null? slist) (values (append newls slist) #f)]
							  [(symbol? (car slist))
							  		(if [proc (car slist) old]
							  			(values (append (append newls (list new)) (cdr slist)) #t)
							  			(helper (cdr slist) (append newls (list (car slist)))))]
							  [else (mv-let ([inner changed?] [helper (car slist) (list)])
							  			(let ([outter (append newls (list inner))])
							  				(if changed?
							  					(values (append outter (cdr slist)) #t)
							  					(helper (cdr slist) outter))))]))
					  (lambda (a b) a))])