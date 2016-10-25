;Name: Fangyuan Wang
;Assignment 6

;#1
(define curry2
	[lambda (p)
		(lambda (x)
			(lambda (y)
				(p x y)))])

;#2
(define curried-compose
	[lambda (p)
		(lambda (x)
			(lambda (y)
				(p (x y))))])

;#3
(define compose
	[lambda L
		(lambda (x)
			(if [null? (cdr L)]
				[(car L) x]
				[(car L) ((apply compose (cdr L)) x)]))])

;#4
(define make-list-c
	[lambda (n)
		(lambda (ls)
			(if [zero? n]
				'()
				(cons ls ((make-list-c (- n 1)) ls))))])

;#5 helper method
;to get the list of the value of input parameters
(define get-list-of-values
	[lambda (ls)
		(map cadr ls)])

;#5 helper method
;to get the list of variables
(define get-list-of-variables
	[lambda (ls)
		(map car ls)])

;#5 helper method
(define get-application
	[lambda (ls)
		(append '(lambda) (cons (get-list-of-variables (cadr ls)) (cddr ls)))])

;#5
(define let->application
	[lambda (ls)
		(cons (get-application ls) (get-list-of-values (cadr ls)))])

;#6 helper method
(define expand-let
	[lambda (ls newls)
		(if [null? (cdr ls)]
			(list 'let ls newls)
			(list 'let (list (car ls)) (expand-let (cdr ls) newls)))])

;#6
(define let*->let
	[lambda (ls)
		(expand-let (cadr ls) (caddr ls))])


;#7
(define filter-in
	[lambda (pred? ls)
		(if [null? ls]
			'()
			(filter pred? ls))])
;#8
(define filter-out
	[lambda (pred? ls)
		(remp pred? ls)])

;#9
(define sort-list-of-symbols
	[lambda (ls)
		(map string->symbol (list-sort string<? (map symbol->string ls)))])

;#10 helper method
;return the new list
(define invert-helper
	[lambda (ls newls)
		(if [null? ls]
			newls
			(let ((first (caar ls)) (second (cadar ls)))
				(invert-helper (cdr ls) (append newls (list (cons second (list first)))))))])

;#10
(define invert
	[lambda (ls)
		(invert-helper ls '())])

;#11 helper method
(define get-vec-index
	[lambda (i pro ls)
		(if [null? ls]
			#f
			(if [pro (car ls)]
				i
				(get-vec-index (+ 1 i) pro (cdr ls))))])

;#11
(define vector-index
	[lambda (pro vec)
		(get-vec-index 0 pro (vector->list vec))])