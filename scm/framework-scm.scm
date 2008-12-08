
(define-module (scm framework-scm)
  #:export (output-framework)
  )

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (guile)
	     (srfi srfi-1)
	     (ice-9 pretty-print)
	     (srfi srfi-13)
	     (scm page)
	     (lily))

(define-public (output-framework basename book scopes fields )
  (let*
      ((file (open-output-file (format #f "~a.scm" basename))))
    
    (display ";;Creator: LilyPond\n" file)
    (display ";; raw SCM output\n" file)

  (for-each
   (lambda (page)
     (display
       ";;;;;;;;;;;;;;;;;;;;;;;;;;\n;;;PAGE\n" file)
;     (pretty-print (ly:stencil-expr page) file)
     (write (ly:stencil-expr page) file)
     )
   (map page-stencil (ly:paper-book-pages book)))))

(define-public output-classic-framework output-framework)


(define-public (convert-to-ps . args) #t)
(define-public (convert-to-pdf . args) #t)
(define-public (convert-to-png . args) #t)
