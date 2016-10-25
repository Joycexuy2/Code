(define times3
    (lambda (x)
       (* x 3)))

(define times6
    (lambda (x)
       (* 2 (times3 x))))

(define make-adder
	(lambda (m)
		(lambda (n)
			(+ m n))))

(define add5 (make-adder 5))
> add5
#<procedure>
> (add5 7)
12
((make-adder 8) 7)
15
> 

((lambda (m) 
	(lambda (n)
	(+ m n))
	4
9))

;factorial
(define fact 
	(lambda (n)
		(if  (zero? n)
			1
			(* n fact (- n 1)))))

