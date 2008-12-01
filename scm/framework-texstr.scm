;;;; framework-tex.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

(define-module (scm framework-texstr)
  #:export (output-framework-tex	
	    output-classic-framework-tex))

(use-modules (ice-9 regex)
	     (scm paper-system)
	     (ice-9 string-fun)
	     (guile)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))

(define format ergonomic-simple-format)

(define (header filename)
  (format  "% header
\\input{lilypond-tex-metrics}
\\documentclass{article}
\\lilyglobalscale{1.0}
\\lilymetricsfile{~a.textmetrics}
\\begin{document}
" filename))

(define (footer)
  "
\\end{document}
")

(define-public (output-classic-framework basename book scopes fields)
  (let* ((filename (format #f "~a.texstr" basename))
	 (outputter (ly:make-paper-outputter
		     (open-file filename "wb")
		     (ly:get-option 'backend)))
	 (paper (ly:paper-book-paper book))
	 (lines (ly:paper-book-systems book)))
    (ly:outputter-dump-string outputter (header basename))
    (for-each
     (lambda (system)
       (ly:outputter-dump-stencil outputter (paper-system-stencil system)))
     lines)
    (ly:outputter-dump-string outputter (footer))))

(define-public (output-framework basename book scopes fields )
  (let* ((filename (format #f "~a.texstr" basename))
	 (outputter
	  (ly:make-paper-outputter
	   (open-file filename "wb")
	   (ly:get-option 'backend)))
	 (paper (ly:paper-book-paper book))
	 (pages (ly:paper-book-pages book)))
    (ly:outputter-dump-string outputter (header basename))
    (for-each
     (lambda (page)
       (ly:outputter-dump-stencil outputter page))
     pages)
    (ly:outputter-dump-string outputter (footer))))

(define-public (convert-to-ps . args) #t)
(define-public (convert-to-pdf . args) #t)
(define-public (convert-to-png . args) #t)
(define-public (convert-to-dvi . args) #t)
(define-public (convert-to-tex . args) #t)
