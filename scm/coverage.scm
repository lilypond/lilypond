(define-module (scm coverage))

(use-modules (lily)
	     (ice-9 rdelim)
	     (ice-9 format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (coverage:show-all)
  (newline)
  (hash-fold
   (lambda (key val acc)
     (if (string-contains key "lilypond")
	 (begin
	   (format #t
		 "
Coverage for file: ~a
"
		 key)
	 (display-coverage key val)))
     #t)
   #t
   coverage-table))

(define-public (coverage:enable)
  (trap-set! memoize-symbol-handler record-coverage)
  (trap-enable 'memoize-symbol)
  (trap-enable 'traps))

(define-public (coverage:disable)
  (trap-set! memoize-symbol-handler #f)
  (trap-disable 'memoize-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define coverage-table (make-hash-table 57))

(define (read-lines port)
  (string-split (read-delimited "" port) #\newline))

(define (display-coverage file vec)
  (let*
      ((lines (read-lines (open-file file "r"))))

    (do
	((i 0 (1+ i))
	 (l lines (cdr l)))
	((or (null? l) (>= i (vector-length vec))))

      (display (format #f "~8a: ~a\n"
		       (if (vector-ref vec i)
			   "#t"
			   "") (car l))))))

(define (record-coverage key cont exp env)
  (let*
      ((name (source-property exp 'filename))
       (line (source-property exp 'line))
       (vec (and name (hashv-ref coverage-table name #f)))
       (veclen (and vec (vector-length vec)))
       (veccopy (lambda (src dst)
		  (vector-move-left! src 0 (vector-length src)
				     dst 0)
		  dst)))
    (if (and line name)
	(begin
	  (if (or (not vec) (>= line (vector-length vec)))
	      (set! vec
		    (hashv-set! coverage-table name
				(if vec
				    (veccopy vec (make-vector (1+ line) #f))
				    (make-vector (1+ line) #f)))))

	  (vector-set! vec line #t))
    )))





