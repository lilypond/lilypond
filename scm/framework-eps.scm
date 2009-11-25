;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

(define-module (scm framework-eps))

;;; this is still too big a mess.

(use-modules (ice-9 regex)
	     (ice-9 string-fun)
	     (guile)
	     (scm framework-ps)
	     (scm paper-system)
	     (scm page)
	     (scm output-ps)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (lily))

(define format
  ergonomic-simple-format)

(define framework-eps-module
  (current-module))

(define (widen-left-stencil-edges stencils)
  "Change STENCILS to use the union for the left extents in every
stencil so that LaTeX's \\includegraphics command doesn't modify the
alignment."
  (define left
    (if (pair? stencils)
	(apply min
	       (map (lambda (stc)
		      (interval-start (ly:stencil-extent stc X)))
		    stencils))
	0.0))

  (map (lambda (stil)
	 (ly:make-stencil
	  (ly:stencil-expr stil)
	  (cons left
		(cdr (ly:stencil-extent stil X)))
	  (ly:stencil-extent stil Y)))
       stencils))

(define (dump-stencils-as-EPSes stencils book basename)
  (define do-pdf
    (member  "pdf" (ly:output-formats)))

  (define paper
    (ly:paper-book-paper book))
    
  (define create-aux-files
    (ly:get-option 'aux-files))

  (define (dump-infinite-stack-EPS stencils)
    (let* ((dump-me (stack-stencils Y DOWN 2.0 stencils)))
      (dump-stencil-as-EPS paper dump-me basename #t)))

  (define (dump-counted-stencil stencil-count-pair)
    "Return EPS filename."
    (let* ((stencil (car stencil-count-pair))
	   (number (cdr stencil-count-pair))
	   (name (format "~a-~a" basename number)))
      (dump-stencil-as-EPS paper stencil name
			   (ly:get-option 'include-eps-fonts))
      (string-append name ".eps")))

  ;; main body
  ;; First, create the output, then if necessary, individual staves and 
  ;; finally write some auxiliary files if desired
  (dump-infinite-stack-EPS stencils)
  (postprocess-output book framework-eps-module
			(format "~a.eps" basename) (ly:output-formats))

  ;; individual staves (*-1.eps etc.); only print if more than one stencil
  ;; Otherwise the .eps and the -1.eps file will be identical and waste space
  ;; Also always create if aux-files=##t
  (if (or create-aux-files (< 1 (length stencils)))
    (let* ((widened-stencils (widen-left-stencil-edges stencils))
	   (counted-systems  (count-list widened-stencils))
	   (eps-files (map dump-counted-stencil counted-systems)))
      (if do-pdf
	  ;; par-for-each: a bit faster ...
	  (for-each (lambda (y) (postscript->pdf 0 0 y))
		    eps-files))))

  ;; Now, write some aux files if requested: .texi, .tex and .count
  ;; for direct inclusion into latex and texinfo
  (if create-aux-files
    (let* ((write-file (lambda (str-port ext)
			 (if create-aux-files
		           (let* ((name (format "~a-systems.~a" basename ext))
			          (port (open-output-file name)))
			     (ly:message (_ "Writing ~a...") name)
			     (display (get-output-string str-port) port)
			     (close-output-port port)))))
	   (tex-system-port (open-output-string))
	   (texi-system-port (open-output-string))
	   (count-system-port (open-output-string)))
      (for-each (lambda (c)
		  (if (< 0 c)
		      (display (format
			        "\\ifx\\betweenLilyPondSystem \\undefined
  \\linebreak
\\else
  \\expandafter\\betweenLilyPondSystem{~a}%
\\fi
" c)
			       tex-system-port))
		  (display (format "\\includegraphics{~a-~a}%\n"
				   basename (1+ c)) tex-system-port)
		  (display (format "@image{~a-~a}\n"
				   basename (1+ c)) texi-system-port))
	        (iota (length stencils)))
      (display "@c eof\n" texi-system-port)
      (display "% eof\n" tex-system-port)
      (display (format "~a" (length stencils)) count-system-port)
      (write-file texi-system-port "texi")
      (write-file tex-system-port "tex")
      ;; do this as the last action so we know the rest is complete if
      ;; this file is present.
      (write-file count-system-port "count"))))

(define-public (output-classic-framework basename book scopes fields)
  (output-scopes scopes fields basename)
  (if (ly:get-option 'dump-signatures)
      (write-system-signatures basename (ly:paper-book-systems book) 1))
  (dump-stencils-as-EPSes (map paper-system-stencil
			       (ly:paper-book-systems book))
			  book
			  basename))

(define-public (output-framework basename book scopes fields)
  (output-scopes scopes fields basename)
  (if (ly:get-option 'clip-systems)
      (clip-system-EPSes basename book))
  (dump-stencils-as-EPSes (map page-stencil
			       (ly:paper-book-pages book))
			  book
			  basename))

; redefine to imports from framework-ps
(define convert-to-pdf
  convert-to-pdf)

(define convert-to-ps
  convert-to-ps)

(define convert-to-png
  convert-to-png)
