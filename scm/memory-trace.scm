(define-module (scm memory-trace))

(define-public (mtrace:start-trace freq)
  (set! usecond-interval (inexact->exact (/ 1000000 freq)))
  (call-with-new-thread start-install-tracepoint))

(define-public (mtrace:stop-trace)
  (set! continue-tracing #f))

(define-public mtrace:trace-depth 12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define trace-points '())
(define continue-tracing #t)
(define busy-tracing #f)
(define trace-thread #f)

(define trace-count 0)
(define usecond-interval 100000)
(define (arg-procedure args)
  (if (and (pair? args)
	   (pair? (cdr args))
	   (pair? (cadr args)))
      (caadr args) #f))

(define (record-stack key continuation . args)
  (if (eq? (current-thread) trace-thread)
      #t ;; do nothing.
      (begin
	(set! busy-tracing #t)
	(trap-disable 'traps)
	(trap-disable 'enter-frame)

	(set! trace-count (1+ trace-count))

	(set! trace-points
	      (cons (list
		     (assoc 'total-cells-allocated (gc-stats))
		     (cons 'stack (extract-trace continuation))
		     (cons 'proc (arg-procedure args))
		     (cons 'time (tms:utime (times)))
		     )
		    
		    trace-points))

	(set! busy-tracing #f))))

(define (start-install-tracepoint)
  (set! trace-thread (current-thread))
  (install-tracepoint))

(define (install-tracepoint)
  (if busy-tracing
      (display "last trace not finished yet\n" (current-error-port))
      (begin
	(trap-set! enter-frame-handler record-stack)
	(trap-enable 'enter-frame)
	(trap-enable 'traps)))
  
  (usleep usecond-interval)
  (if continue-tracing
      (install-tracepoint)))

(define-public (mtrace:dump-results base)
  (define out-graph (open-output-file (format #f "~a.graph" base)))
  (define stacks-out (open-output-file (format #f "~a.stacks" base)))
  (define i 0)
  (define last-mem 0)
  
  (format out-graph "# memory trace with ~a points\n" (length trace-points))
  
  (for-each
   (lambda (r)
     (let*
	 ((mem (cdr (assoc 'total-cells-allocated r)))
	  (proc (cdr (assoc 'proc r)))
	  (stack (cdr (assoc 'stack r)))
	  (time (cdr (assoc 'time r))))
       
       (format out-graph "~a ~a\n" time mem)
       (if stack
	   (begin
	     (format stacks-out "~15a - delta-mem: ~15a - ~a \n" i
		     (- mem last-mem) proc)
	     (do
		 ((j 0 (1+ j))
		  (stack (cdr (assoc 'stack r)) stack))
		 ((>= j (vector-length stack)))
	       
	       (format stacks-out "\t~a\n"
		       (vector-ref stack j)))))
       
       (set! i (1+ i))
       (set! last-mem mem)
       ))
   (reverse trace-points)))



(define (test-graph . rest)
  (mtrace:start-trace 100)
  (iota 100000)
  (mtrace:stop-trace)
  (mtrace:dump-results "test"))



(define (extract-trace continuation)
  (let*
      ((stack (make-stack continuation))
       (depth (min (stack-length stack) mtrace:trace-depth))
       (trace (make-vector depth #f)))

    (do
	((i 0 (1+ i)))
	((>= i depth))

      (vector-set!
       trace i
       (let*
	   ((source (frame-source (stack-ref stack i))))

	   (and source
		(cons (source-property source 'filename)
		      (source-property source 'line))))))

    trace))


       
       
