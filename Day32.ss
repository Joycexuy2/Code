(load "chez-init.ss")

(define-datatype kontinuation kontinuation?
  [init-k]
  [append-k (the-car symbol?)
	    	(k kontinuation?)]
  [flatten-cdr-k (the-car (lambda (x) (or (symbol? x) (list? x))))
			(k kontinuation?)]
  [flatten-car-k (flattened-cdr (list-of symbol?))
			(k kontinuation?)]
 )

(define apply-k 
  (lambda (k v)
		(cases kontinuation k
	       	[init-k ()  ;v is the final result
				(pretty-print v)
				(read-flatten-print)]
	       	[append-k (the-car k) ; v is the appended cdr
				(apply-k k (cons the-car v))]
	        [flatten-cdr-k (the-car k)
				(if (list? the-car)
		   			 (flatten-cps the-car
		   			 			  (flatten-car-k v k)) ;v is the flattened cdr
				 	
		    		 (apply-k k (cons the-car v)))]
			[flatten-car-k (flattened-cdr k)      ;v is the flattened car
				    (append-cps v flattened-cdr k)])))

(define flatten-cps
  (lambda (ls k)
    (if (null? ls)
	(apply-k k ls)
	(flatten-cps (cdr ls)
		     (flatten-cdr-k (car ls) k)))))

(define append-cps 
  (lambda (L1 L2 k)
    (if (null? L1)
	(apply-k k L2)
	(append-cps (cdr L1)
		    L2
		    (append-k (car L1) k)))))

(define read-flatten-print
  (lambda ()
    (display "enter slist to flatten: ")
    (let ([slist (read)])
      (unless (eq? slist 'exit)
	(flatten-cps slist (init-k))))))






;(trace append-cps flatten-cps apply-k append-k)............................................