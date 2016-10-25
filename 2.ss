;Name: Fangyuan Wang
;Assignment 2

;#1a
(define fact 
	(lambda (n)
		[if (zero? n)
			1
			(* n [fact (- n 1)])]))

;#1b
(define choose
	[lambda (x y)
		(/ [fact x] (* (fact y) [fact (- x y)]))])

;#2
(define range
	[lambda (x y)
		(if (<= y x)
			'()
			(append (list x) (range [+ 1 x] y)))])

;#3
(define set?
	[lambda (x)
		(if [not (list? x)]
			#f
			(if [null? x]
				#t
				(and [equal? #f (member [car x] [cdr x])] [set? (cdr x)])))])

;#4
(define sum-of-squares 
	[lambda (lon)
		(if [null? lon]
			0
			(+ [* (car lon) (car lon)] [sum-of-squares (cdr lon)] ))])

;#5
(define make-vec-from-points 
	[lambda (x y)
		(list [- (car y) (car x)] 
			  [- (cadr y) (cadr x)] 
			  [- (caddr y) (caddr x)])])

;#6
(define dot-product
	[lambda (x y)
		(+ [* (car x) (car y)] 
		   [* (cadr x) (cadr y)] 
		   [* (caddr x) (caddr y)])])

;#7 Helper Method
;to get the sqaure of the input
(define sq
	[lambda (n)
		(* n n)])

;#7
(define vec-length 
	[lambda (v)
		(sqrt 
			[+ (sq [car v]) 
			   (sq [cadr v]) 
			   (sq [caddr v])])])

;#8
(define distance 
	[lambda (x y)
		(vec-length (make-vec-from-points x y))])

;#9
(define cross-product 
	[lambda (x y)
		(list [- (* [cadr x] [caddr y]) (* [caddr x] [cadr y])] 
			  [- (* [caddr x] [car y]) (* [car x] [caddr y])] 
			  [- (* [car x] [cadr y]) (* [cadr x] [car y])])])

;#10
(define parallel? 
	[lambda (x y)
		(zero? [vec-length (cross-product x y)])])

;#11
(define collinear? 
	[lambda (x y z)
		(and [parallel? (make-vec-from-points x y) (make-vec-from-points x z)] 
			 [parallel? (make-vec-from-points x y) (make-vec-from-points y z)] 
			 [parallel? (make-vec-from-points x z) (make-vec-from-points y z)])])