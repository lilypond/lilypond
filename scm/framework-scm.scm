
(define-module (scm framework-scm)
 #:export (output-framework)
  )

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (srfi srfi-1)
	     (ice-9 pretty-print)
	     (srfi srfi-13)
	     (lily))

(define-public (output-framework outputter book scopes fields basename)
  (ly:outputter-dump-string outputter ";; raw SCM output\n")

  (for-each
   (lambda (page)
     (ly:outputter-dump-string
      outputter ";;;;;;;;;;;;;;;;;;;;;;;;;;\n;;;PAGE\n") 
     (ly:outputter-dump-string
      outputter
      (call-with-output-string
       (lambda (port)
	 (pretty-print (ly:stencil-expr page) port)))))
   (ly:paper-book-pages book)))


(define-public (convert-to-ps . args) #t)
(define-public (convert-to-pdf . args) #t)
(define-public (convert-to-png . args) #t)
(define-public (convert-to-dvi . args) #t)
(define-public (convert-to-tex . args) #t)
