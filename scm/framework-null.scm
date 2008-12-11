;; module for benchmarking.

(define-module (scm framework-null)
  #:export (output-framework)
  )

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (guile)
	     (srfi srfi-1)
	     (ice-9 pretty-print)
	     (srfi srfi-13)
	     (lily))

(define-public (output-framework channel book scopes fields)

  
  #t)

(define-public output-classic-framework output-framework)


(define-public (convert-to-ps . args) #t)
(define-public (convert-to-pdf . args) #t)
(define-public (convert-to-png . args) #t)
