;Name: Fangyuan Wang
;Assignment 3

;methods from A2
(define make-vec-from-points 
	[lambda (x y)
		(list [- (car y) (car x)] 
			  [- (cadr y) (cadr x)] 
			  [- (caddr y) (caddr x)])])

;to get the sqaure of the input
(define sq
	[lambda (n)
		(* n n)])

(define vec-length 
	[lambda (v)
		(sqrt 
			[+ (sq [car v]) 
			   (sq [cadr v]) 
			   (sq [caddr v])])])

;#1 Helper method
;use recursion
(define nearest-point-finder
	[lambda (len p y n)
		(if (null? y)
			n
			[if (< (vec-length [make-vec-from-points p (car y)]) len)
				(nearest-point-finder 
					(vec-length [make-vec-from-points p (car y)]) p (cdr y) (car y))
				(nearest-point-finder len p (cdr y) n)])])

;#1
(define nearest-point 
	[lambda (p y)
		(nearest-point-finder 
			(vec-length (make-vec-from-points p (car y))) 
			  p 
			  (cdr y) 
			  (car y))])

;#2
(define union
	[lambda (x y)
		(enum-set->list [make-enumeration (append x y)])])


;#3 HelperMethod
;to find the intersection of two sets using recursion
(define intersection-finder
	[lambda (x y z)
		(if (null? x)
			z
			[if (equal? #f (member [car x] y))
				(intersection-finder (cdr x) y z)
				(intersection-finder (cdr x) y (append z (list (car x))))])])

;#3
(define intersection 
	[lambda (x y)
		(intersection-finder x y '())])

;#4
(define subset?
	[lambda (s1 s2)
		(if (null? s1)
			#t
			(and [not (equal? #f (member (car s1) s2))] [subset? (cdr s1) s2]))])

;method from assignment 2
(define set?
	[lambda (x)
		(if [not (list? x)]
			#f
			(if [null? x]
				#t
				(and [equal? #f (member [car x] [cdr x])] [set? (cdr x)])))])

;#5 HelperMethod
;to find the if the pair only contain two elements
(define only-two-element?
	[lambda (n p)  ;n: number of elements so far, p: current pair
		(if [null? p]
			(< n 3)
			(only-two-element? (+ n 1) (cdr p)))])

;#5
(define relation?
	[lambda (x)
		(if [not(list? x)]
			#f
			(if [null? x]
				#t
				[if [or (not(set? x)) (not(pair? (car x)))]
					#f
					(and (only-two-element? 0 (car x)) (relation? (cdr x)))]))])

;#5 Helper Method
;extract the first element out
(define get-domain
	[lambda (ls r)   ;ls: list of domain  r:the given relation
		(if [null? r]
			ls
			(if [equal? #f (member (caar r) ls)]
				(get-domain (append ls (list (caar r))) (cdr r))
				(get-domain ls (cdr r))))])

;#6
(define domain
	[lambda [r]
		(if [null? r]
			'()
			[get-domain '() r])])


;#7 helper method
;find the pair in form of (a a) in the relation
(define reflexive-use-domain?
	[lambda (d r)
		(if [null? d]
			#t
			(if [equal? #f (member (list (car d) (car d)) r)]
				#f
				(reflexive-use-domain? (cdr d) r)))])

;#7
(define reflexive?
	[lambda (r)
		(if [null? r]
			#t
			[reflexive-use-domain? (domain r) r])])

;#8 Helper method
(define step-counter
	[lambda (n c)
		(if [equal? n 1]
			c
			[if (equal? 0 (modulo n 2))
				(step-counter (/ n 2) (+ 1 c))
				(step-counter (+ 1 (* 3 n)) (+ 1 c))])])

;#8
(define hailstone-step-count
	[lambda (n)
		(if [equal? n 1]
			0
			(step-counter n 0))])