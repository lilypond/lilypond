(use-modules
 (ice-9 regex))

(define (format-c-header c-h)
  (regexp-substitute/global
   #f "," 
   (regexp-substitute/global #f "(SCM|\\)|\\() *" c-h 'pre "" 'post)
   'pre " " 'post)
  )

(define (document-scheme-function name c-header doc-string)
  (string-append
   "@defun " (symbol->string name)  " " (format-c-header c-header) "\n"
   doc-string
   "\n@end defun\n\n")
   )

(define all-scheme-functions
   (hash-fold
    (lambda (key val prior)
      (cons (cons key val)  prior)
      )
    '() (ly:get-all-function-documentation))
   )

(define (all-scheme-functions-doc)
  (let*
      (

       (fdocs (map (lambda (x)
		(document-scheme-function (car x) (cadr x) (cddr x))
		)
	      all-scheme-functions)
	 )
       (sfdocs (sort fdocs string<?))
       )


    (make <texi-node>
      #:name "Scheme functions"
      #:desc "Primitive functions exported by LilyPond"
      #:text
      (apply string-append sfdocs)
	) 
  ))


; (dump-node (all-scheme-functions-doc)  (current-output-port) 0 )
