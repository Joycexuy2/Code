;Name: Fangyuan Wang
;Assignment 5

;#1 helper method
;to determine if two intervals intersects
(define intersects? 
	(lambda (x y)
		(>= (cadr x) (car y))))

;#1 helper method
;sort the interval list
(define minimize-sort-list
	[lambda (ls)
		(cond [(null? (cdr ls)) ls]
			  [(intersects? (car ls) (cadr ls))
			  	(minimize-sort-list (cons (union-interval (car ls) (cadr ls)) (cddr ls)))]
			  [else (cons (car ls) (minimize-sort-list (cdr ls)))])])

;#1 helper method
;to union two intervals
(define union-interval 
	[lambda (x y)
		(if (>= (cadr x) (cadr y))
			x
			(list (car x) (cadr y)))])

;#1
(define minimize-interval-list
	[lambda (ls)
		(minimize-sort-list (sort (lambda (x y) 
									[< (car x) (car y)]) ls))]) 

;#2
(define exists?
	[lambda [pred? ls]
		(if [null? ls]
			#f
			(or [pred? (car ls)] [exists? pred? (cdr ls)]))])

;#3 helper method
;to keep track of the index that pred returns true
(define get-index
	[lambda [pred? ls i]
		(cond ([null? ls] i)
			([pred? (car ls)] i)
			(else (get-index pred? (cdr ls) (+ 1 i))))])

;#3
(define list-index
	[lambda [pred? ls]
		(if [null? ls]
			#f
			(if [pred? (car ls)]
				0
				(let ((x (get-index pred? (cdr ls) 1)))
					(if [>= (get-index pred? (cdr ls) 1) (length ls)]
					#f
					x))))])

;#4 helper method
;to do factorial of N
(define factorial
	[lambda (n)
		(if [zero? n]
			1
			(* n (factorial (- n 1))))])

;#4 Helper method
(define get-comb
	[lambda (n k)
		(/ [factorial n] [* (factorial k) (factorial (- n k))])])

;#4 Helper method
;generate sequence
(define get-sequence
	[lambda (n k ls)
		(if [equal? n k]
			'(1)
			(cons [get-comb n k] [get-sequence n (+ 1 k) '()]))])

;#4
(define pascal-triangle 
	[lambda (n)
		(if [negative? n]
			'()
			(cons (get-sequence n 0 '()) (pascal-triangle (- n 1))))])

;#5 helper method
;multiply an element by a list
(define mul-by-list
	[lambda (e ls)
		(if [null? ls]
			'()
			(cons [list e (car ls)] [mul-by-list e (cdr ls)]))])

;#5
(define product
	[lambda (s1 s2)
		(if [or (null? s1) (null? s2)]
			'()
			(append (mul-by-list (car s1) s2) (product (cdr s1) s2)))])


;#6
(define max-edges
	[lambda (n)
		(/ (* n (- n 1)) 2)])

;#7 helper method
;to determine if the vertex in the list
(define not-in-list?
	[lambda (v ls)
		(if [null? ls]
			#t
			(equal? #f (member v ls)))])

;#7 Helper method
;extract the first element out
(define get-domain
	[lambda (ls r)   ;ls: list of domain  r:the given relation
		(if [null? r]
			ls
			(if [equal? #f (member (caar r) ls)]
				(get-domain (append ls (list (caar r))) (cdr r))
				(get-domain ls (cdr r))))])

;#7 helper method
(define subset?
	[lambda (s1 s2)
		(if (null? s1)
			#t
			(and [not (equal? #f (member (car s1) s2))] [subset? (cdr s1) s2]))])

;#7 helper method
;test if the edges satify conditions
(define legal-edges?
	[lambda (v d ls)
		(cond [(null? ls) #t]
			  [(not-in-list? v ls) (subset? (remove v d) (car ls))]
			  [else (#f)])])

;#7
(define complete?
	[lambda (g)
		(if [or (null? g) (equal? 1 [length g])]
			#t
			(let check ([v (caar g)] [d (get-domain '() g)] [ls g])
					(if [not (null? (cdr ls))]
						(and (legal-edges? v d (cdar ls)) (check (caadr ls) d (cdr ls)))
						#t)))])

;#7 prof's version
; (define complete?
;   (lambda (g)
;     (andmap (lambda (x)
;        (= (length (cadr x)) (- (length g) 1)))
;      g)))


;#8 helper method
;to get the list of edges
(define get-edges
	[lambda (v ls)
		(cons v (list (remove v ls)))])

;#8
(define complete
	[lambda (ls)
		(cond ([null? ls] '() )
			  ([null? (cdr ls)] (list (cons (car ls) (list '()))))
			  (else (let edges ([v (car ls)] [vertices ls] [lst ls])
				(if [null? lst]
					'()
					(if [null? (cdr lst)]
						(cons [get-edges v vertices] [edges (car lst) vertices (cdr lst) ])
						(cons [get-edges v vertices] [edges (cadr lst) vertices (cdr lst)]))))))])

;#9
(define replace
	[lambda (old new ls)
		(if [null? ls]
			'()
			(if [equal? old (car ls)]
				(append (list new) (replace old new (cdr ls)))
				(append (list (car ls)) (replace old new (cdr ls)))))])

;#10 helper method
;to check if it is the first occurrence
(define first-occur
	[lambda (element ls newls occur)
		(if [equal? #t occur]
			(if [null? ls]
				newls
				(append newls ls))
			(if [null? ls]
				newls
				(if [equal? element (car ls)]
					(first-occur element (cdr ls) newls #t)
					(first-occur element (cdr ls) (append newls (list (car ls))) #f))))])

;#10
(define remove-first 
	[lambda (element ls)
		(first-occur element ls '() #f)])

;#11
(define remove-last
	[lambda (element ls)
		(reverse (remove-first element (reverse ls)))])