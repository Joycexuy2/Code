;Name: Fangyuan Wang
;Assignment 7


;#1 helper method
(define copy-vector
	[lambda (start copy-index old-v new-v)
		(if [zero? (vector-length old-v)]
			new-v
			(begin (vector-set! new-v start (vector-ref old-v copy-index))
				   (if [equal? (+ 1 copy-index) (vector-length old-v)]
				   		new-v
				   		(copy-vector (+ 1 start) (+ 1 copy-index) old-v new-v))))])

(define copy-list
	[lambda (start copy-index old-ls new-v)
		(if [zero? (length old-ls)]
			new-v
			(begin (vector-set! new-v start (list-ref old-ls copy-index))
				   (if [equal? (+ 1 copy-index) (length old-ls)]
				   		new-v
				   		(copy-list (+ 1 start) (+ 1 copy-index) old-ls new-v))	))])

;#1
(define vector-append-list
	[lambda (v ls)
		(copy-list (vector-length v)
					0
					ls
					(copy-vector 0 0 v (make-vector (+ (vector-length v) (length ls)))))])

;#2 helper
;left sort
(define leftsort
	[lambda (pred ls pivot)
		(if [null? ls]
			ls
			(if [pred pivot (car ls)]
				(leftsort pred (cdr ls) pivot)
				(cons (car ls) (leftsort pred (cdr ls) pivot))))])

;#2 helper
;sort the partition of list to the right of the pivot
(define rightsort
	[lambda (pred ls pivot)
		(if [null? ls]
			ls
			(if [pred pivot (car ls)]
				(cons (car ls) (rightsort pred (cdr ls) pivot))
				(rightsort pred (cdr ls) pivot)))])

;#2
(define qsort
	[lambda (pred ls)
		(cond [(null? ls) ls]
			  [(null? (cdr ls)) ls]
			  [else (append 
			  				(qsort pred (leftsort pred (cdr ls) (car ls)))
			  				(list (car ls))
			  				(qsort pred (rightsort pred (cdr ls) (car ls))))])])

;#3 helper
(define get-set-from-list
	[lambda (ls)
		(if [null? ls]
			ls
			(if (member (car ls) (cdr ls))
				(get-set-from-list (cdr ls))
				(append (get-set-from-list (cdr ls)) (list (car ls)))))])

;#3 helper
(define get-neighbor
	[lambda (v ls)
		(cond [(null? ls) '()]
			  [(eq? v (caar ls)) (cadar ls)]
			  [else (get-neighbor v (cdr ls))])])

;#3 helper
(define get-reached
	[lambda (reached ls)
		(if [null?  reached]
			'()
			(get-set-from-list (append 
									   (list (car reached))
									   (get-neighbor (car reached) ls)
									   (get-reached (cdr reached) ls))))])

;#3
(define connected?
	[lambda (g)
		(cond [(null? g) #t]
			  [(null? (cdr g)) #t]
			  [else (let ver-list ([visited (cadr (car g))] [lst g])
			  	         (letrec ([ver-ls (get-reached visited lst)])
			  	         	(if [equal? (length visited) (length ver-ls)]
			  	         		(equal? (length ver-ls) (length (map car g)))
			  	         		(ver-list ver-ls g))))])])

;#4
(define reverse-it 
	[lambda (ls)
		(cond [(null? ls) ls]
			  [(null? (cdr ls)) ls]
			  [else (let get-rev ([ls ls] [newls '()])
			  			(if (null? ls)
			  				newls
			  				(get-rev (cdr ls) (cons (car ls) newls))))])])

;#5-1
(define empty-BST
	[lambda ()
		'()])
;#5-2
(define empty-BST?
	[lambda (obj)
		(and (null? obj) (list? obj))])
;#5-3
(define BST-insert
	[lambda (num bst)
		(cond [(empty-BST? bst) (list num (empty-BST) (empty-BST))]
			  [(= (car bst) num) bst]
			  [(< (car bst) num) (list (car bst) 
			  					       (BST-left bst) 
			  					       (BST-insert num (BST-right bst)))]
			  [(> (car bst) num) (list (car bst)
			  					       (BST-insert num (BST-left bst))
			  					       (BST-right bst))])])
;#5-4
(define BST-inorder
	[lambda (bst)
		(if [null? bst]
			(list)
			(append (BST-inorder (BST-left bst))
					(list (car bst))
					(BST-inorder (BST-right bst))))])
;#5-5
(define BST?
	[lambda (obj)
		(cond [(not(list? obj)) #f]
			  [(null? obj) #t]
			  [(not (= 3 (length obj))) #f]
			  [(not(and [number? (car obj)]
			  		[BST? (BST-left obj)]
			  		[BST? (BST-right obj)])) #f]
			  [(not (apply < (BST-inorder obj))) #f]
			  [else #t])])

;#5-6
(define BST-element
	[lambda (bst)
		(car bst)])
;#5-7
(define BST-left
	[lambda (bst)
		(list-ref bst 1)])
;#5-8
(define BST-right
	[lambda (bst)
		(list-ref bst 2)])

;#5-9
(define BST-insert-nodes
	[lambda (bst nums)
		[if [null? nums]
			bst
			(BST-insert-nodes (BST-insert (car nums) bst) (cdr nums))]])

;#5-10
(define BST-contains?
	[lambda (bst num)
		(cond [(empty-BST? bst) #f]
			  [(equal? (BST-element bst) num) #t]
			  [else (or (BST-contains? (BST-left bst) num)
			  			(BST-contains? (BST-right bst) num))])])

;#6
(define map-by-position
	[lambda (fn-list arg-list)
		(map (lambda (fn arg) (fn arg)) fn-list arg-list)])

;#7-1
(define bt-leaf-sum
	[lambda (T)
		(if [number? T]
			T
			(+ (bt-leaf-sum (list-ref T 1)) (bt-leaf-sum (list-ref T 2))))])

;#7-2
(define bt-inorder-list
	[lambda (T)
		(if [number? T]
			'() 
			(append (bt-inorder-list (list-ref T 1))
					(list (car T))
					(bt-inorder-list (list-ref T 2))))])

;#7-3
(define bt-max
	[lambda (T)
		(if [number?  T]
			T
			(max (bt-max (list-ref T 1)) (bt-max (list-ref T 2))))])

;#7-4 helper method
(define bt-max-interior-helper
	[lambda (T)
		(letrec ([left-tree (list-ref T 1)] [right-tree (list-ref T 2)])
			(cond [(and (number? left-tree) (number? right-tree))
					(letrec ([leaf-sum (bt-leaf-sum T)])
						(cons (cons (car T) leaf-sum) (cons (car T) leaf-sum)))]
				  [(and (list? left-tree) (number? right-tree))
				  	(letrec ([left-max (bt-max-interior-helper left-tree)])
				  		(letrec ([left-max-sum (cdar left-max)] 
				  				 [left-sum (cddr left-max)])
				  			(letrec ([total-sum (+ left-sum right-tree)])
				  				(if (< left-max-sum total-sum)
				  					(cons (cons (car T) total-sum) (cons (car T) total-sum))
				  					(cons (car left-max) (cons (car T) total-sum))))))]
				  [(and (list? right-tree) (number? left-tree))
				  	(letrec ([right-max (bt-max-interior-helper right-tree)])
				  		(letrec ([right-max-sum (cdar right-max)]
				  				 [right-sum (cddr right-max)])
				  			(letrec ([total-sum (+ right-sum left-tree)])
				  				(if (< right-max-sum total-sum)
				  					(cons (cons (car T) total-sum) (cons (car T) total-sum))
				  					(cons (car right-max) (cons (car T) total-sum))))))]
				  [else (letrec ([right-max (bt-max-interior-helper right-tree)] 
				  				 [left-max (bt-max-interior-helper left-tree)])
				  			(letrec ([right-max-sum (cdar right-max)] 
				  					 [right-sum (cddr right-max)] 
				  					 [left-max-sum (cdar left-max)] 
				  					 [left-sum (cddr left-max)])
				  				(letrec ([total-sum (+ left-sum right-sum)])
				  					(if [< left-max-sum right-max-sum]
				  						(if [< right-max-sum total-sum]
				  							(cons (cons (car T) total-sum) (cons (car T) total-sum))
				  							(cons (car right-max) (cons (car T) total-sum)))
				  						(if [< left-max-sum total-sum]
				  							(cons (cons (car T) total-sum) (cons (car T) total-sum))
				  							(cons (car left-max) (cons (car T) total-sum)))))))]))])


;#7-4
(define bt-max-interior
	[lambda (T)
		(caar (bt-max-interior-helper T))])