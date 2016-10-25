(define rember ; may be useful for the determinant problem
  (lambda (set value)
    (cond [(null? set) '()]
	  [(equal? (car set) value)
	   (rember (cdr set) value)]
	  [else (cons (car set)
		      (rember (cdr set) value))])))

;---- auxiliary procedure that you are allowed to use when implementing trace.

(define display-traced-output
  (let ([multi-indent-string
	 (lambda (level)
	   (let loop ([level level] [result ""])
	     (if (zero? level)
		 result
		 (loop (- level 1) (string-append result "| ")))))])
  (lambda args   ; (level proc-name args) or (level answer)
    (let ([indent-string (multi-indent-string (car args))])
      (display indent-string)
      (display (if (= 2(length args))
		   (cadr args)
		   (cons (cadr args) (caddr args)))))
      (newline))))

'(display-traced-output 5 'f '(a b c))
'(display-traced-output 3 4)

;------------------- Examples for trace problem on computer part.
; ---  After loading your submitted interpreter code and typing (rep), these should work as expected: 


(define aa (lambda (n) (+ 1 n)))
(define bb (lambda (n m) (+ m (aa n))))
(define cc (lambda (n) (+ n (bb n (aa n)))))
(trace aa)
(trace bb)
(trace cc)
(cc 3)

(trace +)
(define aa (lambda (n) (+ 1 n)))
(define bb (lambda (n m) (+ m (aa n))))
(define cc (lambda (n) (+ n (bb n (aa n)))))
(trace aa)
(trace bb)
(trace cc)
(cc 3)


(define tree-mult
  (lambda (ls)
    (cond 
     [(null? ls) 1]
     [(not (list? (car ls)))
      (* (car ls) (tree-mult (cdr ls)))]
     [else (* (tree-mult (car ls)) (tree-mult (cdr ls)))])))
(trace tree-mult)
(tree-mult '((1 2) (() (3) 4)))
