;;;; framework-tex.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c)  2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-module (scm framework-texstr)
  #:export (output-framework-tex	
	    output-classic-framework-tex))

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))


(define (define-fonts paper)
   ;; UGH. FIXME.
   (format "(globalscale ~a)\n" (ly:paper-outputscale paper)))


(define-public (output-framework outputter book scopes fields basename )
  (let* ((paper (ly:paper-book-paper book))
	 (pages (ly:paper-book-pages book))
	 )
    (ly:outputter-dump-string outputter (define-fonts paper))
    (for-each
     (lambda (page)
       (ly:outputter-dump-stencil outputter page))
     pages)))



(define-public (convert-to-ps . args) #t)
(define-public (convert-to-pdf . args) #t)
(define-public (convert-to-png . args) #t)
(define-public (convert-to-dvi . args) #t)
(define-public (convert-to-tex . args) #t)
