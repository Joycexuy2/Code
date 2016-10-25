(define largest-in-lists
	[lambda (ls)
		(if [null?  (apply append ls)]
			#f
			(apply max (apply append ls)))])