;;;; coverage.scm

(define-module (scm coverage))

(use-modules (lily)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (ice-9 format) ;; needed for ~8@ 
	     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (coverage:show-all filter?)
  (let*
      ((keys
	(filter filter?
		(sort (map car (hash-table->alist coverage-table)) string<? ))))
    
  (newline)
  (for-each
   (lambda (k)

     (format #t "Coverage for file: ~a\n" k)
     (display-coverage
      k (hash-ref coverage-table k)
      (format #f "~a.cov" (basename k))))
   keys)))


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

(define (display-coverage file vec out-file)
  (let*
      ((lines (read-lines (open-file file "r")))
       (format-str "~8@a: ~5@a:~a\n")
       (out (if out-file (open-output-file out-file)
		(current-output-port))))

    (format out format-str "-" 0 (format #f "Source:~a" file))
    (do
	((i 0 (1+ i))
	 (l lines (cdr l)))
	((or (null? l) ))

      (format out format-str
		       (cond
			((and (< i (vector-length vec)) (vector-ref vec i)) "1")
			((and (string-contains file ".ly") (string-match "^[ \t]*%.*$" (car l)))
			 "-")
			((string-match  "^[ \t]*[()'`,]*$" (car l))
			 "-")
			((string-match  "^[ \t]*;.*$" (car l))

			 "-")
			(else "0"))
		       (1+ i)
		       (car l)))))

(define (record-coverage key cont exp env)
  (let*
      ((name (source-property exp 'filename))
       (line (source-property exp 'line))
       (vec (and name (hash-ref coverage-table name #f)))
       (veclen (and vec (vector-length vec)))
       (veccopy (lambda (src dst)
		  (vector-move-left! src 0 (vector-length src)
				     dst 0)
		  dst)))
    (if (and line name)
	(begin
	  (if (or (not vec) (>= line (vector-length vec)))
	      (set! vec
		    (hash-set! coverage-table name
				(if vec
				    (veccopy vec (make-vector (1+ line) #f))
				    (make-vector (1+ line) #f)))))

	  (vector-set! vec line #t))
    )))





