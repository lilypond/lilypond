(define-module (scm memory-trace))


(define-public (mtrace:start-trace freq)
  (set! usecond-interval (inexact->exact (/ 1000000 freq)))
  (call-with-new-thread start-install-tracepoint))

(define-public (mtrace:stop-trace)
  (set! continue-tracing #f))

(define-public mtrace:trace-depth 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define trace-points '())
(define continue-tracing #t)
(define busy-tracing #f)
(define trace-thread #f)

(define trace-count 0)
(define usecond-interval 100000)

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
		     (assoc 'total-cells-allocated  (gc-stats))
		     (cons 'stack (extract-trace continuation)))
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
  (define out (open-output-file (format #f "~a.graph" base)))
  (define stacks-out (open-output-file (format #f "~a.stacks" base)))
  (define i 0)

  (format out "# memory trace with ~a points\n" (length trace-points))
  
  (for-each
   (lambda (r)
     (format out "~a ~a\n" i
	     (cdr (assoc 'total-cells-allocated r)))

     (if (assoc 'stack r)
	 (format stacks-out "~a: ~a\n"
		 i
		 (cdr (assoc 'stack r))))
     
     (set! i (1+ i)))
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






       
       
