;;;; framework-ps.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

(define-module (scm framework-eps))

;;; this is still too big a mess.

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (ice-9 format)
	     (guile)
	     (scm framework-ps)
	     (scm paper-system)
	     (scm page)
	     (scm output-ps)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))

(define framework-eps-module (current-module))


(define (widen-left-stencil-edges stencils)
  "Change STENCILS to use the union for the left extents in every
stencil, so LaTeX includegraphics doesn't fuck up the alignment."

  (define left
    (apply min
	   (map (lambda (stc)
		  (interval-start (ly:stencil-extent stc X)))
		stencils)))

  (map (lambda (stil)
	 
	 (ly:make-stencil
	  (ly:stencil-expr stil)
	  (cons
	   left
	   (cdr (ly:stencil-extent stil X)))
	  (ly:stencil-extent stil Y)
	  ))
       stencils))

  
(define (dump-stencils-as-EPSes stencils book basename)
  (define do-pdf (member  "pdf" (ly:output-formats)))
  (define paper (ly:paper-book-paper book))
  (define (dump-infinite-stack-EPS stencils)
    (let* ((dump-me (stack-stencils Y DOWN 2.0 stencils)))
      (dump-stencil-as-EPS paper dump-me basename #t)))

  (define (dump-stencils-as-separate-EPS stencils count )
    (if (pair? stencils)
	(let* ((line (car stencils))
	       (rest (cdr stencils))
	       (system-base-name (format "~a-~a" basename count))
	       )

	  (dump-stencil-as-EPS
	   paper line system-base-name
	   (ly:get-option 'include-eps-fonts))

	  (if do-pdf
	      (postscript->pdf  0 0  (string-append system-base-name ".eps")))
	  (dump-stencils-as-separate-EPS rest (1+ count)))))


  ;; main body 
  (let* ((tex-system-name (format "~a-systems.tex" basename))
	 (texi-system-name (format "~a-systems.texi" basename))
	 (tex-system-port (open-output-file tex-system-name))
	 (texi-system-port (open-output-file texi-system-name)))
    
    (ly:message (_ "Writing ~a...") tex-system-name)
    (ly:message (_ "Writing ~a...") texi-system-name)

    (set! stencils (widen-left-stencil-edges stencils))
    
    (dump-stencils-as-separate-EPS stencils 1)
    (for-each (lambda (c)
		(if (< 0 c)
		    (display (format "\\ifx\\betweenLilyPondSystem \\undefined
  \\linebreak
\\else
  \\betweenLilyPondSystem{~a}
\\fi
" c) tex-system-port))
		(display (format "\\includegraphics{~a-~a}\n"
				 basename (1+ c)) tex-system-port)
		(display (format "@image{~a-~a}\n"
				 basename (1+ c)) texi-system-port))
	      (iota (length stencils)))
    
    (display "@c eof - 'eof' is a Makefile marker; do not remove. " texi-system-port)
    (display "% eof - 'eof' is Makefile marker; do not remove. " tex-system-port)
    
    (close-output-port texi-system-port)
    (close-output-port tex-system-port)
    
    (dump-infinite-stack-EPS stencils)
    (postprocess-output book framework-eps-module
			(format "~a.eps" basename) (ly:output-formats))))



(define-public (output-classic-framework basename book scopes fields)
  (output-scopes scopes fields basename)

  (if (ly:get-option 'dump-signatures)
      (write-system-signatures basename (ly:paper-book-systems book) 1))
  
  (dump-stencils-as-EPSes
   (map paper-system-stencil (ly:paper-book-systems book))
   book
   basename))

(define-public (output-framework basename book scopes fields)
  (output-scopes scopes fields basename)
  (dump-stencils-as-EPSes
   (map page-stencil (ly:paper-book-pages book)) book basename))
  

; redefine to imports from framework-ps
(define convert-to-pdf convert-to-pdf)
(define convert-to-ps convert-to-ps)
(define convert-to-png convert-to-png)
(define convert-to-tex convert-to-tex)
(define convert-to-dvi convert-to-dvi)


