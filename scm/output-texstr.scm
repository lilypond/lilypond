;;;; texstr.scm -- implement Scheme output routines for TeX strings
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-module (scm output-texstr))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (srfi srfi-13)
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
  
(define-public (text font s)
  (call-with-output-string
   (lambda (port)
     (write (list
	     (ly:font-file-name font)
	     (ly:font-magnification font)
	     s) port)
     (newline port))))
