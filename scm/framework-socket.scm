;;;; framework-socket.scm

(define-module (scm framework-socket)
  #:export (output-framework)
  )

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (scm paper-system)
	     (ice-9 format)
	     (guile)
	     (srfi srfi-1)
	     (ice-9 pretty-print)
	     (srfi srfi-13)
	     (lily))

(define-public (output-framework channel book scopes fields )
  (let*
      ((ctor-arg (if (string? channel)
		     (open-output-file (format "~a.socket" channel))
		     channel))
       (outputter (ly:make-paper-outputter
		   ctor-arg
		   'socket))
       (systems (ly:paper-book-systems book)))

    (if (pair? systems)
	(ly:outputter-dump-stencil outputter
				   (paper-system-stencil (car systems))))
    ))

(define-public output-classic-framework output-framework)


(define-public (convert-to-ps . args) #t)
(define-public (convert-to-pdf . args) #t)
(define-public (convert-to-png . args) #t)
