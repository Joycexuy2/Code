; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
	(lambda (proc-names idss bodies old-env)
		(recursively-extended-env-record proc-names idss bodies old-env)))	
	
(define extend-env-recursively-improper
	(lambda (proc-names improper proper old-env)
		(recursively-extended-env-record-improper proc-names improper proper old-env)))
	
(define extend-env-improper
  (lambda (syms impropersym vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))
(define get-improper-last (lambda (lst)
	(if (symbol? lst)
		lst
		(get-improper-last (cdr lst)))))
		
(define get-improper-first (lambda (lst)
	(if (symbol? lst)
		'()
		(cons (car lst)(get-improper-first (cdr lst))))))
	
	
(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var 
  ;is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env) ;replaces the arguments with the paramaters passed in
		(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail))))
	  (recursively-extended-env-record (procnames idss bodies old-env)
		(let 
			([pos (list-find-position sym procnames)])
			(if (number? pos)
				(closure 
					(list-ref idss pos)
					(list(list-ref bodies pos))
					env)
				(apply-env old-env sym succeed fail))))
	  (recursively-extended-env-record-improper (procnames idss bodies old-env)
		(let 
			([pos (list-find-position sym procnames)])
			(if (number? pos)
				(if (and (not (list? (list-ref idss pos)))(pair? (list-ref idss pos)))
					(closure-improper 
						(get-improper-first(list-ref idss pos))
						(get-improper-last(list-ref idss pos))
						(list(list-ref bodies pos))
						env)
					(closure 
						(list-ref idss pos)
						(list(list-ref bodies pos))
						env))
				(apply-env old-env sym succeed fail)))))))
