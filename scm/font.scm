;;;; font.scm -- implement Font stuff
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2000--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-public (magstep x)
  (exp (* (/ x 6) (log 2))))

;; Should separate default sizes
;; into separate list/alist ?

;; FIXME docstring for paper20-font-vector 
  """ Entries have the following format

  ( #(SERIES SHAPE FAMILY)  .
    (DEFAULT-SIZE . #(SIZE-FONT-ENTRY... ) ))

where SIZE-FONT-ENTRY is

  (DESIGN-SIZE FONT-NAME)

or

  (DESIGN-SIZE (FONT-NAME1 FONT-NAME2 ... ))"""
  
(define-public paper20-font-vector
  '((#(medium upright number) .
     (10 . #((10.0  . "feta-nummer10"))))
    (#(medium upright roman) .
     (10.0 . #((6.0 . "cmr6")
	       (8.0 . "cmr8") 
	       (10.0 . "cmr10")
	       (17.0 . "cmr17")
	       )))
    (#(* * music). 
     (20.0 . #((11.22 . ("feta11" "parmesan11"))
	       (12.60 . ("feta13" "parmesan13"))
	       (14.14 . ("feta14" "parmesan14"))
	       (15.87 . ("feta16" "parmesan16"))
	       (17.82 . ("feta18" "parmesan18"))
	       (20.0 . ("feta20" "parmesan20"))
	       (22.45 . ("feta23" "parmesan23"))
	       (25.20 . ("feta26" "parmesan26"))
	       )))
    (#(medium upright sans) .
     (10.0  . #((8.0 . "cmss8")
		(10.0 . "cmss10")
		(12.0 . "cmss12")
		(17.0 . "cmss17")
		)))
    (#(medium upright typewriter) .
     (10.0 . #((8.0 .  "cmtt8")
	       (10.0 . "cmtt10")
	       (12.0 . "cmtt12")
	       )))
    (#(bold italic roman) .
     (10.0 . #((8.0 . "cmbxti8")
	       (10.0 . "cmbxti10")
	       (14.0 . "cmbxti14")
	       )))
    (#(medium italic roman) .
     (10.0 . #((7.0 . "cmti7")
	       (10.0 . "cmti10")
	       (12.0 . "cmti12")
	       )))
    (#(bold upright roman) .
     (10.0 . #((6.0 . "cmbx6")
	       (8.0 . "cmbx8")
	       (10.0 . "cmbx10")
	       (12.0 . "cmbx12")
	       )))
    (#(bold-narrow upright roman) .
     (10.0 . #((10.0 . "cmb10")
	       )))
    (#(medium caps roman) .
     (10.0 . #((10.0 . "cmcsc10"))))
    (#(* * dynamic) .
     (14.0 .  #((6.0 . "feta-din6")
		(8.0 . "feta-din8")
		(10.0 . "feta-din10")
		(12.0 . "feta-din10")
		(14.0 . "feta-din14")
		(17.0 . "feta-din17")
		)))
    (#(* * math) .
     (10.0 . #((10.0 . "msam10"))))
    ;; testing ps-encoding
    (#(medium latin1 roman) .
     (10.0 . #((12.0 . "ecrm12"))))
    (#(bold latin1 roman) .
     (10.0 . #((14.0 . "ecbm14"))))))

(define (scale-font-entry entry factor)
  (cons
   (car entry)
   (cons (* (cadr entry) factor)
	 (cddr entry))))

(define size-independent-fonts
  '((#(* * braces) .
     (10 . #((10.0 . ("feta-braces00"
		      "feta-braces10"
		      "feta-braces20"
		      "feta-braces30"
		      "feta-braces40"
		      "feta-braces50"
		      "feta-braces60"
		      "feta-braces70"
		      "feta-braces80")))))))

(define-public (scale-font-list factor)
  (append size-independent-fonts
	  (map (lambda (y) (scale-font-entry y factor)) paper20-font-vector)))


