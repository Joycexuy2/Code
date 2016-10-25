;Fangyuan Wang
;Exam 1

;#1 helper
(define member?
	[lambda (x ls)
		(and (member x ls) #t)])
;#1
(define contains-both?
	[lambda (lon sym1 sym2)
		(and (member? sym1 lon) (member? sym2 lon))])

;#2
(define make-vec-iterator
	[lambda (v)
		(let ([current 0])
			(lambda (msg . args)
				(case msg
					[(val) (vector-ref v current)]
					[(set-val!) 
				 			(vector-set! v current (car args))]
					[(next) (set! current (+ 1 current))]
				 	[(prev) (set! current (- current 1))])))])

;#3 helper
(define sum-row
	[lambda (r1 r2)
		(if [null?  r1]
			'()
			(append (list (+ (car r1) (car r2))) (sum-row (cdr r1) (cdr r2))))])

;#3 helper
(define sum-helper
	[lambda (m1 m2 m3)
		(if [null? m1]
			m3
			(sum-helper (cdr m1) (cdr m2) (append m3 (list (sum-row (car m1) (car m2))))))])

;#3
(define matrix-sum
	[lambda (m1 m2)
		(sum-helper m1 m2 (list))])

;#4 helper method
(define next-row
	[lambda (prev-row)
		(append '(1) 
			(map (lambda (x y) (+ x y)) prev-row (append (cdr prev-row) '(0))))])

;#4
(define pascal-triangle
	[lambda (n)
		(cond [(< n 0) '()]
			  [(= n 0) (list '(1))]
			  [else (let ([triangle-n-1 (pascal-triangle (- n 1))])
			  			(cons (next-row (car triangle-n-1)) triangle-n-1))])])