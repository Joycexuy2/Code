;Fangyuan Wang

(define apply-continuation
	[lambda (k . v)
		(apply k v)])


(define subst-left-cps
  (lambda (new old slist succeed fail)
    (trace-let loop ([slist slist] [succeed succeed] [fail fail])
               (cond
                [(null? slist) (fail)]
                [(list? (car slist))
                 (loop (car slist)
                       (lambda (substituted-car)
                         (succeed (cons substituted-car (cdr slist))))
                       (lambda ()
                         (loop (cdr slist)
                               (lambda (substituted-cdr)
                                 (succeed (cons (car slist) substituted-cdr)))
                               fail)))]
                [else (if (eq? (car slist) old)
                          (succeed (cons new (cdr slist)))
                          (loop (cdr slist)
                                (lambda (substituted-cdr)
                                  (succeed (cons (car slist) substituted-cdr)))
                                fail))]))))


(define subst-left-cps-2args
	[lambda (new old slist equal-pred? k)
		(let helper ([slist slist] [k k])
				(lambda (slist k)
					(cond [(null? slist) (apply-continuation k #f slist)]
						  [(list? (car slist))
						  	  (helper (car slist)
						  	  	      (lambda (car-result)
						  	  	      		(if (car car-result)
						  	  	      			(apply-continuation k #t (cons (cdr car-result) (cdr slist)))
						  	  	      			(helper (cdr slist)
						  	  	      				    (lambda (cdr-result)
						  	  	      				    	(apply-continuation k (car cdr-result) (cons (car slist) (cdr cdr-result))))))))]
						  [else (if (equal-pred? (car slist) old)
						  			(apply-continuation k #t (cons new (cdr slist)))
						  			(helper (cdr slist)
						  				    (lambda (cdr-result)
						  				    	(apply-continuation k (car cdr-result) (cons (car slist) (cdr cdr-result))))))])))])