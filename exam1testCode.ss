;; Test code for CSSE 304 Assignment Exam 1 201630

(define (test-contains-both?)
    (let ([correct '(
		    #t
		    #t
		    #t
		    #t		     
		     )]
          [answers 
            (list 
	     (and (contains-both? '(a b c a b d) 'c 'd) 
		  (not (contains-both? '(a b c a b d) 'a 'e) ))
	     (and (contains-both? '(a b c a b d) 'd 'c) 
		  (not (contains-both? '(a b c a b) 'e 'a) ))
	     (and (contains-both? '(a a c a b d) 'a 'b) 
		  (not (contains-both? '(a b c a b d) 'x 'y) ))
	     (and (contains-both? '(a a c b b d) 'b 'a) 
		  (not (or (contains-both? '(a) 'x 'y) 
			   (contains-both? '() 'x 'y)) ))	     
	     )])
      (display-results correct answers equal?)))

(define (test-make-vec-iterator)
    (let ([correct '(
		     (b a)
		     (a a)
		     (d a)
		     (b d)
		     (b d e)
		     (b d e a)
		     )]
          [answers 
            (list 
	     (let* ([v '#(a b c)] 
		    [vi (make-vec-iterator v)] 
		    [x (vi 'val)] 
		    [y (begin (vi 'next) (vi 'val))]) 
	       (list y x))
	     (let* ([v '#(a b c)] 
		    [vi (make-vec-iterator v)] 
		    [x (vi 'val)] 
		    [y (begin (vi 'next) 
			      (vi 'prev) 
			      (vi 'val))]) 
	       (list y x))
	     (let* ([v '#(a b c)] 
		    [vi (make-vec-iterator v)] 
		    [x (vi 'val)] 
		    [y (begin (vi 'next) 
			      (vi 'next) 
			      (vi 'set-val! 'd) 
			      (vi 'val))]) 
	       (list y x))
	     (let* ([v '#(a b c)] 
		    [vi (make-vec-iterator v)] 
		    [x (begin (vi 'next) 
			      (vi 'val))] 
		    [y (begin (vi 'next) 
			      (vi 'set-val! 'd) 
			      (vi 'val))]) 
	       (list x y))
	     (let* ([v '#(a b c)] 
		    [vi (make-vec-iterator v)] 
		    [x (begin (vi 'next) 
			      (vi 'val))] 
		    [y (begin (vi 'next) 
			      (vi 'set-val! 'd) 
			      (vi 'prev) 
			      (vi 'set-val! 'e) 
			      (vi 'next) 
			      (vi 'val))] 
		    [z (begin (vi 'prev) 
			      (vi 'val))]) 
	       (list x y z))
	     (let* ([v '#(a b c)] 
		    [vi (make-vec-iterator v)] 
		    [x (begin (vi 'next) 
			      (vi 'val))] 
		    [y (begin (vi 'next) 
			      (vi 'set-val! 'd) 
			      (vi 'prev) 
			      (vi 'set-val! 'e) 
			      (vi 'next) (vi 'val))] 
		    [v2 (make-vec-iterator v)] 
		    [x2 (begin (v2 'next) 
			       (v2 'val))] 
		    [y2 (begin (vi 'prev) 
			       (vi 'prev) 
			       (v2 'set-val! 
				   (vi 'val)) 
			       (v2 'val))]) 
	       (list x y x2 y2))
	     )])
      (display-results correct answers equal?)))

(define (test-matrix-sum)
    (let ([correct '(
		     ((5 2 6) (11 13 15))
		     ((5))
		     ((4) (4) (4))
		     ((1 3 5 7 9))
		     )]
          [answers 
            (list 
	     (matrix-sum '((1 2 3)(4 5 6)) '((4 0 3) (7 8 9)))
	     (matrix-sum '((3)) '((2)))
	     (matrix-sum '((3) (2) (1)) '((1) (2) (3)))
	     (matrix-sum '((1 2 3 4 5)) '((0 1 2 3 4)))
)])
      (display-results correct answers equal?)))

(define (test-pascal-triangle)
    (let ([correct '(
		     ((1 3 3 1) (1 2 1) (1 1) (1))
		     ((1 4 6 4 1) (1 3 3 1) (1 2 1) (1 1) (1))
		     ((1 1) (1))
		     ((1))
		     )]
          [answers 
            (list 
	     (pascal-triangle 3)
	     (pascal-triangle 4)
	     (pascal-triangle 1)
	     (pascal-triangle 0)
	     )])
      (display-results correct answers equal?)))




;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))
    
(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
	    ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)
	   
	  
     



;You can run the tests individually, or run them all
;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'contains-both?) 
  (test-contains-both?)
  (display 'make-vec-iterator) 
  (test-make-vec-iterator)
  (display 'matrix-sum) 
  (test-matrix-sum)
  (display 'pascal-triangle) 
  (test-pascal-triangle)    
)

(define r run-all)

