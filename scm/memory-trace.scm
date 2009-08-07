;;;; memory-trace.scm

(define-module (scm memory-trace))
(use-modules (lily)
	     (ice-9 format))

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
(define start-time 0)
(define start-memory 0)

(define trace-count 0)
(define usecond-interval 100000)
(define (arg-procedure args)
  (if (and (pair? args)
	   (pair? (cdr args))
	   (pair? (cadr args)))
      (caadr args) #f))
(define last-count 0)
(define (record-stack key continuation . args)
  (if (eq? (current-thread) trace-thread)
      #t ;; do nothing.
      (let*
	  ((cells (cdr (assoc 'total-cells-allocated (gc-stats))))
	   (proc (arg-procedure args))
	   (time (tms:utime (times)))
	   (stack (extract-trace continuation)))
	
	(set! busy-tracing #t)
	(trap-disable 'traps)
	(trap-disable 'enter-frame)

	(set! trace-count (1+ trace-count))
 	(ly:progress "<~a: ~a/~a>\n"
		     trace-count
		     (- time start-time)
		     (- cells last-count))

	(set! last-count cells)
	(set! trace-points
	      (cons (list
		     (cons 'cells cells)
		     (cons 'proc proc)
		     (cons 'stack stack)
		     (cons 'time time)
		     )
		    
		    trace-points))

	(set! busy-tracing #f))))

(define (start-install-tracepoint)
  (set! trace-thread (current-thread))
  (set! trace-points '())
  (set! continue-tracing #t)
  (set! trace-count 0)
  (set! start-memory (cdr (assoc 'total-cells-allocated (gc-stats))))
  (set! start-time (tms:utime (times)))
  
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
  (let*
      ((stacks-name (format #f "~a.stacks" base))
       (graph-name (format #f "~a.graph" base))
       (graph-out (open-output-file graph-name))
       (stacks-out (open-output-file stacks-name))
       (i 0)
       (last-mem 0)
       )

    (ly:progress "Memory statistics to ~a and ~a..."
		 stacks-name graph-name)
    (format graph-out "# memory trace with ~a points\n" (length trace-points))
    (for-each
     (lambda (r)
       (let*
	   ((mem (- (cdr (assoc 'cells r)) start-memory))
	    (proc (cdr (assoc 'proc r)))
	    (stack (cdr (assoc 'stack r)))
	    (time (- (cdr (assoc 'time r)) start-time)))
	 
	 (format graph-out "~a ~a\n" time mem)
	 (if stack
	     (begin
	       (format stacks-out "~5a t = ~5a - delta-mem: ~15a - ~a\n" i
		       time
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
     (reverse trace-points))))


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


       
       
