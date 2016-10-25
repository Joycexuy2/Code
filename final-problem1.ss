;Fangyuan Wang

;problem 1
(define two-by-two-determinant ;get the determinate of a 2x2 matrix
	[lambda (m)
		(let [(first (list-ref m 0)) (second (list-ref m 1))]
			(- (* (list-ref first 0) (list-ref second 1)) 
			   (* (list-ref second 0) (list-ref first 1))))])

(define power    ;to get (-1)^p
	[lambda (p)
		(if (zero? p)
			1
			(* (-1) (power (- p 1))))])

(define remove-by-index ;remove a element from the ls by index
	[lambda (ls index current)
		(if (eq? current index)
			(cdr ls)
			(cons (car ls) (remove-by-index (cdr ls) index (+ 1 current))))])

(define delete-row ;cross out a row
	[lambda (m row index)
		(if (zero? row)
			(cdr m)
			(if (eq? row index)
				(cdr m)
				(cons (car m) (delete-row (cdr m) row (+ 1 index)))))])

(define delete-col ;cross out a column
	[lambda (m col)
		(map (lambd (x) (remove-by-index x (- col 1) 0)) m)])

(define produce-minor ;get the minor according to the given row and column
	[lambda (m row col)
		(delete-row (delete-col m col) (- row 1) 0)])

(define cofactors   ;get the cofactors for the given matrix and row
	[lambda (m row max-col)
		(let helper ([col 1])
			(* (pow (+ row col)) (determinant (produce-minor m row col))))])

(define determinant
	[lambda (m)
		(let helper ([row 1] [col 1] [minors '()])
			(if (= (length m) 2)
				(two-by-two-determinant m)
				(let ([entries (list-ref m (- row 1))])
					(apply + (map (lambda (x y) (* x y)) 
							 	  entries 
								  (cofactors m row (length (list-ref m (- row 1)))))))))])



;problem 2
(load "chez-init.ss") 
(define-datatype continuation continuation?
	[init-k]
	[tree-cdr-k (the-car (lambda (x) (or (list? x) (number? x))))
			 	(k continuation?)]
	[tree-car-k (the-cdr number?)
	 			(k continuation?)])

(define apply-k
	[lambda (k val)
		(cases continuation k
			[init-k () val]
			[tree-cdr-k (the-car k)
				(if (not (list? the-car))
					(apply-k k (* the-car val))
					(tree-mult-cps the-car
						           (tree-car-k val k)))]
			[tree-car-k (the-cdr k)
					(apply-k k (* the-cdr val))])])

(define tree-mult-cps
	[lambda (ls k)
		(cond [(null? ls) (apply-k k 1)]
			  [else
			  	  (tree-mult-cps (cdr ls) (tree-cdr-k (car ls) k))])])

(define tree-mult
	[lambda (ls)
		(tree-mult-cps ls (init-k))])