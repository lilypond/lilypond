;;;; texstr.scm -- implement Scheme output routines for TeX strings
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

(define-module (scm output-texstr))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (srfi srfi-13)
 (scm framework-tex)
 (lily))

(define (dummy . foo) #f)

(map (lambda (x) (module-define! this-module x dummy))
     (ly:all-stencil-expressions))

(define-public (grob-cause . x) "")
(define-public (no-origin . x) "")
(define-public (placebox
		x y what)
  (if (string? what)
      what
      ""))


(define-public (text font str)
  (call-with-output-string
   (lambda (port)
     (display (format "\\lilygetmetrics{~a~a}{~a}{1.0}{~a}\n"
		      
		      (hash str TEX_STRING_HASHLIMIT)
		    (ly:font-file-name font)
		    (ly:font-file-name font)
		    (sanitize-tex-string str))
	      port)
     )))
