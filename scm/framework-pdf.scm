;; WIP

(define-module (scm framework-pdf))

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))

(define framework-pdf-module (current-module))

(define (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

;;(define pdebug stderr)
(define (pdebug . rest) #f)

(define (pdf-ify lst)
  (cond
   ((pair? lst)
    (cons (pdf-ify (car lst)) (pdf-ify (cdr lst))))
   ((vector? lst)
    (vector-for-each pdf-ify lst))
   ((ly:pdf-object? lst) lst)
   ((or
    (string? lst)
    (number? lst)
    (symbol? lst))
    (pdf-ify lst))
   
   (else
    (ly:make-pdf-object '(null)))
       
  ))

(define (make-page-object parent contents)
  (ly:make-pdf-object
   (cons 'dictionary
	 (pdf-ify
	  `((Type . Page)
	    (Parent . ,parent)
	    (Contents . ,contents)
	    )))))

(define (make-page-node root)  )


(define-public (output-framework basename book scopes fields)
  (let* ((filename (format "~a.pdf" basename))
	 (pdf (ly:open-pdf-file filename))
	 (outputter
	  (ly:make-paper-outputter (format "~a.bla.pdf" basename) "pdf"))


	  
	 (paper (ly:paper-book-paper book))
	 (pages (ly:paper-book-pages book))
	 (landscape? (eq? (ly:output-def-lookup paper 'landscape) #t))
	 (page-number (1- (ly:output-def-lookup paper 'firstpagenumber)))
	 (page-count (length pages))
	 (port (ly:outputter-port outputter))))

    
))
