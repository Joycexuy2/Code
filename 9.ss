;Name: Fangyuan Wang
;Assignment 9

;#1
(define snlist-recur
	[lambda (base-value list-proc non-list-proc)
		(letrec ([helper 
					(lambda (ls)
						(cond [(null? ls) base-value]
							  [(list? (car ls)) (list-proc (helper (car ls)) (helper (cdr ls)))]
							  [else (non-list-proc (car ls) (helper (cdr ls)))]))])
		helper)])

;#1-a
(define sn-list-sum
	(snlist-recur 0 + +))

;#1-b
(define sn-list-map
	[lambda (proc ls)
		((snlist-recur '() cons (lambda (x y) (cons (proc x) y))) ls)])

;#1-c
(define sn-list-paren-count
	[snlist-recur 2 (lambda (x y) (+ x y)) (lambda (x y) y)])

;#1-d
(define sn-list-reverse
	[lambda (ls)
		((snlist-recur '() (lambda (x y) (append y (list x))) 
						   (lambda (x y) (append y (list x)))) ls)])

;#1-e
(define sn-list-occur
	[lambda (s ls)
		((snlist-recur 0 + (lambda (x y) (if (eq? s x)
												(+ 1 y)
												y))) ls)])

;#1-f
(define sn-list-depth
	(snlist-recur 1 (lambda (x y) (max (+ 1 x) y))
					(lambda (x y) y)))

;#2
(define bt-recur
	[lambda (num-proc tree-proc)
		(letrec ([helper (lambda (bt)
							[if (number? bt)
								(num-proc bt)
								(tree-proc (car bt) (helper (cadr bt)) (helper (caddr bt)))])])
				helper)])

;#2-a
(define bt-sum
	[bt-recur + (lambda (x y z) (+ y z))])

;#2-b
(define bt-inorder
	[bt-recur (lambda (x) (list)) (lambda (x y z) (append y (list x) z))])

;#3 helper method
;compose
(define compose
	(case-lambda
		[() (lambda (x) x)]
		[(first . rest)
		   (let ([composed-rest (apply compose rest)])
		   	   (lambda (x) (first (composed-rest x))))]))

;#3
(define make-c...r
	[lambda (st)
		(lambda (ls)
			(let ([composed-proc (apply compose 
								(map (lambda (x) (eval (string->symbol x))) 
									  (map 
										   (lambda (x) (string #\c x #\r))
										   (string->list st))))])
			      (composed-proc ls)))])

;#4 helper method
(define first-sls
	[lambda (s-ls)
		(cond [(null? s-ls) #f]
			  [(list? (car s-ls)) (or (first-sls (car s-ls)) (first-sls (cdr s-ls)))]
			  [else (car s-ls)])])

;#4 helper method
(define rest-sls
	[lambda (s-ls)
		(cond [(null? s-ls) #f]
			  [(list? (car s-ls)) 
			  		(if (rest-sls (car s-ls))
			  			(cons (rest-sls (car s-ls)) (cdr s-ls))
			  			(rest-sls (cdr s-ls)))]
			  [else (cdr s-ls)])])

;#4
(define make-slist-leaf-iterator
	[lambda (s-ls)
		(lambda ()
			(if [not s-ls]
				#f
				(let ([first (first-sls s-ls)] [rest (rest-sls s-ls)])
					 (set! s-ls rest)
					 first)))])