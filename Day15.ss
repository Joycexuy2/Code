



(define-syntax my-let
 (syntax-rules ()
   [(_ ((x v) ...) e1 e2 ...)
    ((lambda (x ...) e1 e2 ...)  v ...)])) 

(my-let ([x 3] [y 2])
  (my-let ([z (+ y 7)] [y (+ x y)])
      (* x y z)))

(define-syntax my-if
	(syntax-rules (then else)
		[(_ e1 then e2) (if e1 e2)]
		[(_ e1 then e2 else e3) (if e1 e2 e3)]))
(my-if 3 then 4)
(my-if #f then 4) ;do not return 4
(my-if #f then 4 else 5) ;return 5


(define-syntax ++
	(syntax-rules ()
		[(_ x) (begin (set! x (+ 1 x))
					  x)]))
(++ 4) ;execption  invalid syntax    can only put variable as x

(define-syntax ++post
	(syntax-rules ()
		[(_ x) (let ([val x])
				  (set! x (+ x 1))
				  val)]))

(define-syntax my-and
	(syntax-rules ()
		[(_) #t]
		[(_ e1) e1]
		[(_ e1 e2 e3 ...)
		 (if e1
		 	 (my-and e2 e3 ...)
		 	 #f)]))

(define-syntax for
	(syntax-rules (:)
		[(_ ((init ...) : test : update ...) body ...)
		 (begin 
		 	init ...
		 	(let loop ()
		 		(if test
		 			(begin
		 				body ...
		 				update ...
		 				(loop)))))]))