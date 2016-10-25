;Name: Fangyuan Wang
;Assignment 8

;#1-a
(define slist-map
	[lambda (proc sls)
		(let ls-map ([proc proc] [sls sls])
			(cond [(null? sls) sls]
			  	  [(symbol? (car sls))
			  	 		(cons (proc (car sls)) 
			  	 		  	  (ls-map proc (cdr sls)))]
			  	  [else (cons (ls-map proc (car sls)) (ls-map proc (cdr sls)))]))])

;#1-b
(define slist-reverse
	[lambda (slist)
		(let rev ([sls slist])
			(cond [(null? sls) (list)]
				  [(symbol? sls) sls]
				  [else (reverse (map rev sls))]))])

;#1-c
(define slist-paren-count
	[lambda (slist)
		(let count ([sls slist])
			(cond [(null? sls) 2]
				  [(symbol? (car sls)) (count (cdr sls))]
				  [else (+ (count (car sls)) (count (cdr sls)))]))])

;#1-d
(define slist-depth
	[lambda (slist)
		(let helper ([sls slist])
			(cond [(null? sls) 1]
				  [(symbol? (car sls)) (helper (cdr sls))]
				  [else (max (+ 1 (helper (car sls))) (helper (cdr sls)))]))])

;#1-e
(define slist-symbols-at-depth
	[lambda (slist d)
		(let get-symbol ([sls slist] [level 1])
			(cond [(null? sls) (list)]
				  [(= level d)
				  	  (if (symbol? (car sls))
				  	  	  (cons (car sls) (get-symbol (cdr sls) level))
				  	  	  (get-symbol (cdr sls) level))]
				  [else 
				  	   (if (symbol? (car sls))
				  	   	   (get-symbol (cdr sls) level)
				  	   	   (append (get-symbol (car sls) (+ 1 level))
				  	   	   		   (get-symbol (cdr sls) level)))]))])

;#2
(define group-by-two
	[lambda (ls)
		(cond [(null? ls) ls]
			  [(= 1 (length ls)) (list ls)]
			  [else (cons (list (car ls) (cadr ls)) (group-by-two (cddr ls)))])])

;#3 helper method
(define get-length-n-list
	[lambda (ls n newls)
		(if [zero? n]
			newls
			(get-length-n-list (cdr ls) (- n 1) (append newls (list (car ls)))))])

;#3 helper method
(define get-the-rest
	[lambda (ls n)
		(if [zero? n]
			ls
			(get-the-rest (cdr ls) (- n 1)))])

;#3
(define group-by-n
	[lambda (ls n)
		(cond [(null? ls) ls]
			  [(<= (length ls) n) (list ls)]
			  [else 
			    (let ([lst (get-length-n-list ls n '())])
			    	(cons lst (group-by-n (get-the-rest ls n) n)))])])

;#4
(define subst-leftmost
	[lambda (new old slist equality-pred?)
		(car (let subst-helper ([slist slist] [newls (list)])
				(cond [(null? slist) (cons (append newls slist) #f)]
				  	  [(symbol? (car slist))
				  			(if [equality-pred? (car slist) old]
				  				(cons (append (append newls (list new)) (cdr slist)) #t)
				  				(subst-helper (cdr slist) (append newls (list (car slist)))))]
				  	  [else (let* ([first (subst-helper (car slist) (list))]
				  			   	   [result (append newls (list (car first)))])
				  				(if (cdr first)
				  					(cons (append result (cdr slist)) #t)
				  					(subst-helper (cdr slist) result)))])))])