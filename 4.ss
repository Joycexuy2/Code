;Name: Fangyuan Wang
;Assignment 4

;#1 helper method
;from assignment 2
(define set?
	[lambda (x)
		(if [not (list? x)]
			#f
			(if [null? x]
				#t
				(and [equal? #f (member [car x] [cdr x])] [set? (cdr x)])))])

;#1 helper method
;to test if is a pair satisfying condition
(define real-pair?
	[lambda (x)
		(and (pair? x) 
			 (null? (cddr x)) 
			 (> (cadr x) 0)
			 (symbol? (car x)))])

;#1 helper method
;to test if every pair in the list satisfying the given condition
(define all-satisfy-pair?
	[lambda (x)
		(if [null? x]
			#t
			(if [real-pair? (car x)]
				(all-satisfy-pair? (cdr x))
				#f))])

;#1 helper method
;to fine if there is duplicate element in the multi-set
(define no-duplicate-element?
	[lambda (obj)
		(and (set? (map car obj)) (pair? (map (lambda (x) x) obj)))])

;#1
(define multi-set?
	[lambda (obj)
		(if [null? obj]
			#t
			(and [all-satisfy-pair? obj] [no-duplicate-element? obj]))])

;#2
(define ms-size
	[lambda (ms) 
		(if [null? ms]
			0
			(apply + [map cadr ms]))])

;#3
(define matrix-ref
	[lambda (m row col)
		(if [zero? row]
			(list-ref (car m) col)
			(matrix-ref (cdr m) (- row 1) col))])

;#4 helper method
;to determine if they have the same length
(define same-length?
	[lambda (len ls)
		(if [null? ls]
			#t
			(and [equal? (length (car ls)) len] [same-length? len (cdr ls)]))])

;to determine if the list contains empty list
(define empty-sublist?
	[lambda (ls)
		(if [null? ls]
			#t
		    (and (null? [car ls]) (empty-sublist? (cdr ls))))])

;to determine if it is a list of list
(define is-a-list?
	[lambda (ls)
		(if [null? ls]
			#t
			(and [list? (car ls)] [is-a-list? (cdr ls)]))])

;#4
(define matrix?
	[lambda (obj)
		(if [or (null? obj) (not (list? obj))]
			#f
			(and (is-a-list? obj) 
				 (not (empty-sublist? obj)) 
				 (same-length? (length (car obj)) (cdr obj))))])

;#5 helper method
(define delete-firsts
	[lambda (ls)
		(if [or (null? ls) (null? (car ls))]
			'()
			(append [map cdr ls]))])

;#5
(define matrix-transpose
	[lambda (m)
		(if [or (null? m) (null? (car m))]
			'()
			(cons [map car m] [matrix-transpose (delete-firsts m)]))])

;#6 helper method
;to keep track of the last element
(define get-last
	[lambda (lt ls)
		(if [null? ls]
			lt
			(get-last (car ls) (cdr ls)))])

;#6
(define last
	[lambda (ls)
		(get-last (car ls) (cdr ls))])

;#7
(define all-but-last
	[lambda (ls)
		(if [null? (cdr ls)]
			'()
			(cons (car ls) (all-but-last (cdr ls))))])