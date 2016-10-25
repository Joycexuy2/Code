;  Continuation-passing style examples


;for factorial accumulator approach is better
(define fact-acc
  [lambda (n acc)
    (if [zero? n]
        acc
        (fact-acc (- n 1) (* n acc)))])

(define print-fact
  (lambda (n)
    (fact-cps n (lambda (v)
		(printf "The factorial of ~s is ~s.~n" n v)))))

(define apply-continuation ;works when we represent a continuation as a scheme procedure
  [lambda (k v)
    (k v)])

(define fact-cps
  (lambda (n k) ;k is continuation
    (if [zero? n]
        (apply-continuation k 1)
        (fact-cps (- n 1)
                  (lambda (v) (apply-continuation k (* n v)))))))
;(print-fact 6)
;(fact-cps 6 ~any proc that takes a single number)

(define list-copy
  (lambda (list)
    (list-copy-cps list 
                   (lambda (x) 
                    (display "The copied list is ") 
                    x))))

(define list-copy-cps
  (lambda (L k)  
    (if (null? L)
        (apply-continuation k (list))
        (list-copy-cps (cdr L) (lambda (cdr-copy)
                                  (apply-continuation k (cons (car L) cdr-copy)))))))
;first approach
(define intersection-cps
   (lambda (los1 los2 k)
      (if [null? los1]
          (apply-continuation k (list))
          (memq-cps (car los1) 
                    los2 
                    (lambda (isin?)
                          (if isin?
                            (intersection-cps (cdr los1)
                                              los2
                                              (lambda (intersection-with-cdr)
                                                (apply-continuation k (cons (car los1) intersection-with-cdr))))
                            (intersection-cps (cdr los1)
                                              los2
                                              k)))))))
  
;second approach
(define intersection-cps
   (lambda (los1 los2 k)
      (if (null? los1) 
          (apply-continuation k (list))
          (intersection-cps (cdr los1)
                             los2
                            (lambda (intersection-with-cdr)
                              (memq-cps (car los1) 
                                        los2
                                        (lambda (isin?)
                                          (apply-continuation k 
                                              (if isin?)
                                                 (cons (car los1) intersection-with-cdr)
                                                  intersection-with-cdr))))))))

(define memq-cps
  (lambda (sym ls k)
    (cond [(null? ls) (apply-continuation k #f)]
          [(eq? sym (car ls))
            (apply-continuation k #t)]
          [else (memq-cps sym (cdr ls) k)])))

'(define remove-cps
  (lambda (sym los k)
))

;; Another approach.  Developed during the second period class.
'(define remove-cps
  (lambda (item ls k)
))

'(define union-cps
  (lambda (los1 los2 k)
))

(define free-vars
  (lambda (exp)
    (cond
      [(symbol? exp) (list exp)]
      [(eq? (1st exp) 'lambda)
       (remove (car (2nd exp))
               (free-vars (3rd exp)))]
      [else (union (free-vars (1st exp))
                   (free-vars (2nd exp)))])))
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
  
'(define free-vars-cps
  (lambda (exp k)
    (cond [(symbol? exp)  (apply-continuation k (list exp))]
          [(eq? (1st exp) 'lambda)
              (free-vars-cps (3rd exp)
                              (lambda (free-in-body)
                                (remove-cps (2nd exp) free-in-body k)))]
          [else (free-vars-cps (1st exp)
                               (lambda (fre-vars-in-rator)
                                  (free-vars-cps)))])))
  


(define list-product
  (lambda (list)
    (prod-cps list
              (lambda (prod) (printf "The product is ~s~n" prod))
              (lambda () (printf "A zero was found, the product is 0 ~n")))))

(define prod-cps
  (lambda (L succeed fail)
    (cond [(null? L) (succeed 1)]
          [(zero? (car L)) (fail)]
          [else (prod-cps (cdr L)
                          (lambda (cdr-prod)
                            (succeed (* (car L) cdr-prod)))
                          fail)])))



(define subst-leftmost
  (lambda (new old slist)
    (subst-left-cps new
                    old
                    slist
                    (lambda (x) x)
                    (lambda () (display "no match for ") old))))


;  Try evaluating (subst-leftmost 1 2 '(((1 (2 3 2) 2) 2) 3 4))


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




;  Here is a more fully-traced version that lets us see the application
;  of the continuations.

(define subst-left-cps
  (lambda (new old slist succeed fail)
    (trace-let loop ([slist slist] [succeed succeed] [fail fail])
      (cond
       [(null? slist) (fail)]
       [(list? (car slist))
        (loop (car slist)
              (trace-lambda subs-car-succeed (substituted-car)
                 (succeed (cons substituted-car (cdr slist))))
              (trace-lambda car-fail ()
                 (loop (cdr slist)
                       (trace-lambda subs-cdr-succeed  (substituted-cdr)
                           (succeed (cons (car slist) substituted-cdr)))
                       fail)))]
       [else (if (eq? (car slist) old)
                 (succeed (cons new (cdr slist)))
                 (loop (cdr slist)
                       (trace-lambda symbol-succeed (substituted-cdr)
                           (succeed (cons (car slist) substituted-cdr)))
                       fail))]))))

;;  > (subst-leftmost 1 2 '((((1 3) (2 3 2) 2) 2) 3 4))
;;  |(loop ((((1 3) (2 3 2) 2) 2) 3 4) #<procedure> #<procedure>)
;;  |(loop (((1 3) (2 3 2) 2) 2) #<procedure> #<procedure>)
;;  |(loop ((1 3) (2 3 2) 2) #<procedure> #<procedure>)
;;  |(loop (1 3) #<procedure> #<procedure>)
;;  |(loop (3) #<procedure> #<procedure>)
;;  |(loop () #<procedure> #<procedure>)
;;  |(car-fail)
;;  |(loop ((2 3 2) 2) #<procedure> #<procedure>)
;;  |(loop (2 3 2) #<procedure> #<procedure>)
;;  |(subs-car-succeed (1 3 2))
;;  |(subs-cdr-succeed ((1 3 2) 2))
;;  |(subs-car-succeed ((1 3) (1 3 2) 2))
;;  |(subs-car-succeed (((1 3) (1 3 2) 2) 2))
;;  |((((1 3) (1 3 2) 2) 2) 3 4)
;;  ((((1 3) (1 3 2) 2) 2) 3 4)

(define cps-list-recur
  (lambda (base proc-cps)
    (letrec ([helper
	      (lambda (ls k)
		(if (null? ls)
		    (k base)
		    (helper (cdr ls)
			    (lambda (cdr-result)
			      (proc-cps (car ls) cdr-result k)))))])
      helper)))

(define list-sum-cps
  (cps-list-recur 0 (lambda (x y k) (k (+ x y)))))

(list-sum-cps '( 4 0 2 5 1) list)
0