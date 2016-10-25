; File /class/cs/cs304/class-examples/engines/engine-intro.ss
;
; Engines abstract the notion of timed pre-emption.  They were first
; introduced by Haynes and Friedman in Computer Languages, 1987.  A
; simpler implementation in which engines are not treated as Scheme
; primitives, but rather are built on top of call/cc was given by 
; Dybvig and Hieb in the same journal in 1989.

; (make-engine thunk) creates an engine that will, when applied to
; three arguments, activate a timer-interrupt mechanism and
; evaluate the body of thunk.  The arguments given to the engine are:
;
; ticks:   a positive integer specifying the amount of "fuel" given to the
;          engine.
; return:  a procedure that specifies what to do if the evaluation of thunk
;          finishes before the fuel expires.  Return is a procedure of two
;          arguments: amount of fuel "left over" and the 
;          result of the computation.
; expire:  a one-argument procedure to be executed if the computation runs
;          out of fuel before the computation completes.  The argument that
;          will be passed to this procedure is a new engine that can
;          finish the computation from where it left off.

; How much computation constitutes a tick?  According to the
; introductory paper, a larger tick count is associated with a larger
; expected amount of computation (in a statistical sense), and unbounded
; real time is associated with an unbounded number of ticks (thus any
; looping construct must consume ticks.  Chez Scheme essentially counts
; procedure calls.

;  Examples are primarily from TSPL and the second paper mentioned above.

(define fib
  (lambda (n)
    (cond [(zero? n) 0]
          [(= n 1) 1]
          [else (+ (fib (sub1 n)) (fib (- n 2)))])))

(define engine-fib
  (lambda (n)
    (make-engine (lambda () (fib n)))))

; > (define eng (engine-fib 6))
; > (eng 50 cons (lambda (new-eng) (set! eng new-eng)))
; > (eng 50 cons (lambda (new-eng) (set! eng new-eng)))
; > (eng 50 cons (lambda (new-eng) (set! eng new-eng)))
; > (eng 50 cons (lambda (new-eng) (set! eng new-eng)))
; > (eng 50 cons (lambda (new-eng) (set! eng new-eng)))
; (47 . 8)
; > 


(define mileage   ; count the ticks
  (lambda (thunk)
    (let loop ([eng (make-engine thunk)]
               [total-ticks 0])
      (eng 50
           (lambda (ticks value)
             (+ total-ticks (- 50 ticks)))
           (lambda (new-engine)
             (loop new-engine (+ 50 total-ticks)))))))

; > (mileage (lambda () (fib 10)))
; 1437
; > (mileage (lambda () (fib 11)))
; 2330
; > (mileage (lambda () (fib 12)))
; 3775
; > (mileage (lambda () (fib 13)))
; 6113
; > (mileage (lambda () (fib 14)))
; 9896


(define round-robin
  (let ([snoc (lambda (L x) (append L (list x)))])
    (lambda (list-of-engines)
      (if (null? list-of-engines)
          '()
          ((car list-of-engines)
           1                    ; it's only allowed to run for one tick.
           (lambda (ticks value)
             (cons value (round-robin (cdr list-of-engines))))
           (lambda (new-engine)
             (round-robin (snoc (cdr list-of-engines) new-engine))))))))
             
; > (round-robin (map engine-fib '(3 7 11 4 12 9 1 6 10 8 2 5)))
; (1 1 2 3 5 8 13 21 34 55 89 144)

; Simulating a multi-tasking operating system

(define make-queue
  (lambda ()
    (cons '() '())))

(define empty-queue?
   (lambda (q)
      (eq? (car q) '())))

(define enqueue
  (lambda (obj q)
    (let ((x (cons obj '())))
      (if (null? (car q))
          (set-car! q x)
          (set-cdr! (cdr q) x))
      (set-cdr! q x)
      q)))


(define dequeue
  (lambda (q)
    (if (null? (car q)) 
        (error 'dequeue "cannot dequeue from empty queue")
        (let ((obj (caar q)))
          (set-car! q (cdar q))
          (if (null? (car q))
              (set-cdr! q '()))
          obj))))

(define time-slice (lambda () (add1 (random 100))))

(define kernel
  (lambda (proc)
     (define ready-queue (make-queue))
     (define start
       (lambda (proc)
          (enqueue (make-engine (lambda () (proc trap)))
                   ready-queue)))
     (define restart
        (lambda (k v)
           (enqueue (make-engine (lambda () (k v))) 
                    ready-queue)))
     (define trap
         (lambda (msg arg)
           (call/cc
             (lambda (k)
                (engine-return
                  (lambda ()
                    (case msg
                      (uninterruptible
                       (restart k (arg)))
                      (start-process
                       (start arg)
                       (restart k #f))
                      (stop-process #f))))))))
     (start proc)
     (let dispatch ()
        (if (empty-queue?  ready-queue)
            'finished
            ((dequeue ready-queue)
             (time-slice)
             (lambda (ticks trap-handler)
               (trap-handler)
               (dispatch))
            (lambda (engine)
               (enqueue engine ready-queue)
               (dispatch)))))))


; An example that uses this multi-tasking simulator

(define amoeba
  (lambda (generation final)
    (lambda (trap)
      (when (< generation final)
            (trap 'uninterruptible
                  (lambda ()
                    (writeout generation)))
            (trap 'start-process (amoeba (+ generation 1) final))
            (trap 'start-process (amoeba (+ generation 1) final)))
      (trap 'stop-process #f))))

(define writeln (lambda x (for-each display x) (newline)))

(define writeout
  (let ([count 0])
    (lambda (n)
      (display n)
      (display " ")
      (set! count (+ count (if (< n 10) 2 3)))
      (when (>= count 66) 
            (newline) 
            (set! count 0)))))


; > (kernel (amoeba 0 6))
; 0 1 2 1 2 3 3 3 2 2 4 3 4 3 3 5 4 4 4 3 3 5 4 4 
; 5 4 4 4 5 5 4 4 4 4 5 5 5 5 5 5 5 5 4 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 5 5 5 ; 5 finished
; > (kernel (amoeba 0 9))
; 0 1 2 1 2 2 3 3 2 3 3 3 4 4 3 3 4 4 4 4 4 3 5 4 5 4 4 4 5 5 5 5 5 
; 5 5 4 4 4 4 5 5 6 5 5 5 5 4 6 6 6 6 6 5 5 6 6 6 6 5 6 5 5 5 5 5 5 6 6 6 6 7
; 6 6 6 6 6 5 6 6 5 5 5 5 7 7 7 7 6 7 6 6 6 6 7 7 6 7 6 6 7 6 6 6 6 6 6 5 6 5
; 5 7 7 7 7 7 7 7 6 7 6 8 7 7 7 7 7 7 7 7 6 6 7 7 7 6 6 6 6 6 6 6 6 8 8 8 7 8 
; 7 7 8 7 7 7 7 7 8 8 7 7 8 7 7 7 7 7 7 7 6 8 7 7 7 7 7 6 6 6 7 6 6 7 6 6 7 6 
; 6 6 6 8 8 8 7 8 8 8 8 8 8 7 7 8 8 7 7 7 8 7 7 8 8 8 8 8 7 8 7 8 8 8 8 7 8 7 
; 7 7 8 8 8 8 7 7 7 7 7 7 7 7 7 7 7 7 7 6 6 7 6 8 8 8 8 8 8 8 8 8 8 8 8 7 8 8 
; 8 7 8 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 7 8 7 7 7 8 8 8 8 8 7 8 7 7 7 7 7 7 8 
; 7 7 7 7 7 7 8 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 
; 8 8 7 8 8 8 8 8 8 8 8 8 7 7 8 7 8 8 8 7 8 8 8 8 7 8 8 7 7 7 8 7 7 7 8 7 7 7 
; 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 
; 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 
; 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 7 8 8 8 8 8 8 8 8 8 8 8 8 7 8 8 8 8 8 8 8 
; 8 7 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 finished




