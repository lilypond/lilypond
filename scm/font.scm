;;;
;;; font.scm -- implement Font stuff
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
;;;

(define style-to-font-alist
  `(
    (finger . ((font-family . number) (font-relative-size . -3)))
    (volta . ((font-family . number) (font-relative-size . -2)))
    (timesig . ((font-family .  number) (font-relative-size . 0)))
    (mmrest . ((font-family . number) (font-relative-size . -1)))
    (mark . ((font-family . number) (font-relative-size . 1)))
    (script . ((font-family . roman) (font-relative-size . -1)))
    (large . ((font-family . roman) (font-relative-size . 1)))
    (Large . ((font-series . bold) (font-family . roman) (font-relative-size . 2)))
    (dynamic . ((font-series . bold) (font-family . dynamic) (font-relative-size . 0)))
))

(define (font-field name font-descr)
      (list-ref
       font-descr
       (cond
	((eq? name 'font-relative-size)  0)
	((eq? name 'font-series) 1)
	((eq? name 'font-shape) 2)
	((eq? name 'font-family) 3)
	((eq? name 'font-name) 4)
	((eq? name 'font-point-size-size) 5)
	)
       ))
  
;; return that part of LIST for which PRED is true.
(define (filter-list pred? list)
  (if (null? list) '()
      (let* (
	     (rest  (filter-list pred? (cdr list)))
	     )
	(if (pred?  (car list))
	    (cons (car list)  rest)
	    rest
	    )
	)
      )
  )

;;;;;;;;; TODO TODO . (should not use filtering?)
;; this is bad, since we generate garbage every font-lookup.
;; otoh, if the qualifiers is narrow enough , we don't generate much garbage.


;; return those descriptions from FONT-DESCR-LIST whose FIELD-NAME matches VALUE
(define (filter-field field-name value font-descr-alist)
      (filter-list
       (lambda (x) (eq? value (font-field field-name (car x))))
       font-descr-alist)
      )

(define paper20-style-sheet-alist
  '(
    ((0 medium upright music feta 20) . "feta20")
    ((-1 medium upright music feta 16) . "feta16")
    ((-2 medium upright music feta 13) . "feta13")
    ((-3 medium upright music feta 13) . "feta11")
    ((-4 medium upright music feta 13) . "feta11")
    ((1 medium upright music feta 23) . "feta23")
    ((2 medium upright music feta 26) . "feta26")
    ((0 medium upright braces feta-braces 20) . "feta-braces20")
    ((0 medium italic roman cmti 10) . "cmti10")
    ((1 medium italic roman cmti 12) . "cmti12")
    ((3 bold italic dynamic feta 10) . "feta-din13")
    ((2 bold italic dynamic feta 10) . "feta-din13")
    ((1 bold italic dynamic feta 10) . "feta-din12")
    ((0 bold italic dynamic feta 10) . "feta-din10")
    ((-1 bold italic dynamic feta 10) . "feta-din8")
    ((-2 bold italic dynamic feta 10) . "feta-din7")
    ((-3 bold italic dynamic feta 10) . "feta-din6")
    ((-4 bold italic dynamic feta 10) . "feta-din5")
    ((-5 bold italic dynamic feta 10) . "feta-din4")
    ((3 medium upright number feta-nummer 13) . "feta-nummer13")
    ((2 medium upright number feta-nummer 13) . "feta-nummer13")
    ((1 medium upright number feta-nummer 12) . "feta-nummer12")
    ((0 medium upright number feta-nummer 10) . "feta-nummer10")
    ((-1 medium upright number feta-nummer 8) . "feta-nummer8")
    ((-2 medium upright number feta-nummer 6) . "feta-nummer6")
    ((-3 medium upright number feta-nummer 5) . "feta-nummer5")
    ((-4 medium upright number feta-nummer 4) . "feta-nummer4")
    ((0 medium upright roman cmr 10) . "cmr10")
    ((1 medium upright roman cmr 12) . "cmr12")
    ((-1 medium upright roman cmr 8) . "cmr8" )
    ((-2 medium upright roman cmr 7) . "cmr7" )
    ((-3 medium upright roman cmr 6) . "cmr6" )
    ((-4 medium upright roman cmr 5) . "cmr5" )
    ((-5 medium upright roman cmr 4) . "cmr4" )
    ((2 bold upright roman cmbx 10) . "cmbx10")
    ((1 bold upright roman cmbx 12) . "cmbx12")
    ((-3 medium upright math msam 10) . "msam10")
    ((-2 medium upright math msam 10) . "msam10")
    ((-1 medium upright math msam 10) . "msam10")
    ((0 medium upright math msam 10) . "msam10")
    ))


;; return a FONT-DESCR with relative size incremented by INCREMENT 
(define (change-relative-size font-desc increment)
  (cons (+ increment (car font-desc)) (cdr font-desc))
  )

;; map a  function FUNC over the keys of an alist LIST, leaving the vals. 
(define (map-alist-keys func list)
  (if (null?  list)
      '()
      (cons (cons (func (caar list)) (cdar list))
	    (map-alist-keys func (cdr list)))
      ))
 
;; map a function FUNC over the vals of  LIST, leaving the keys. 
(define (map-alist-vals func list)
  (if (null?  list)
      '()
      (cons (cons  (caar list) (func (cdar list)))
	    (map-alist-vals func (cdr list)))
      ))

(define (change-style-sheet-relative-size sheet x)
  (map-alist-keys (lambda (descr) (change-relative-size descr  x)) sheet))


;; make style sheet for each paper version.
(define style-sheet-alist
  (map-alist-vals (lambda (x) (change-style-sheet-relative-size
			       paper20-style-sheet-alist x))
		  '((paper11 . -3)
		    (paper13 . -2)
		    (paper16 . -1)
		    (paper20 . 0)
		    (paper23 . 1)
		    (paper26 . 2)
		    ))
  )


(define (font-regexp-to-font-name paper regexp)
  (let ((style-sheet (cdr (assoc paper style-sheet-alist))))
    (let loop ((fonts style-sheet))
      (if (string-match regexp (caar fonts))
	  (cdar fonts)
	  (if (pair? (cdr fonts))
	      (loop (cdr fonts))
	      '())))))

;; reduce the font list by successively applying a font-qualifier.
(define (qualifiers-to-fontname  qualifiers font-descr-alist)
  (if (null? qualifiers)
      (if (null? font-descr-alist)
	  ""  
	  (cdar font-descr-alist))	; return the topmost.
      
      (qualifiers-to-fontname
       (cdr qualifiers)
       (filter-field (caar qualifiers) (cdar qualifiers) font-descr-alist)
      )
  ))

(define (properties-to-font-name paper properties-alist)
  (let*  (
	  (fonts (cdr (assoc paper style-sheet-alist)))

	  ;; change order to change priorities of qualifiers.
	  (q-order    '(font-name font-family font-series font-shape font-point-size font-relative-size))
	  (rawqualifiers (map (lambda (x) (assoc x  properties-alist))
			      q-order))
			
	  (qualifiers (filter-list pair? rawqualifiers))
	  (fontnm (qualifiers-to-fontname qualifiers fonts))
	  (err (current-error-port))
	  )

    (if (eq? fontnm "")
	(begin
	  (display "\ncouldn't find font satisfying " err)
	  (write qualifiers err)
	  (display "\n" err)	  
	  "cmr10"
	  )
	fontnm)
	

    ))


(define markup-abbrev-to-properties-alist
  (append
   '(
     (rows . ((align . 0)))
     (lines . ((align . 1)))
     (roman . ((font-family . roman)))
     (music . ((font-family . music)))
     (finger . ((font-style . finger)))
     (bold . ((font-series . bold)))
     (italic . ((font-shape . italic)))
     (named . ((lookup . name)))
     (super . ((raise . 1) (font-relative-size . -1)))
     (sub . ((raise . -1) (font-relative-size . -1)))
     (text . ((lookup . value)))
     )
   (map (lambda (x) (cons (car x) (cons 'font-style (car x))))
	style-to-font-alist)))
  
(define (markup-to-properties markup)
  ;;(display "markup: `")
  ;;(display markup)
  ;;(display "'\n")
  (if (pair? markup)
      (list markup)
      (let ((entry (assoc markup markup-abbrev-to-properties-alist)))
	(if entry (cdr entry)
	    (list (cons markup #t))))))

; fixme, how's this supposed to work?
; and why don't we import font-setting from elt?
(define (style-to-font-name paper style)
  (let* ((entry (assoc style style-to-font-alist))
	 (qs (if entry (cdr entry) '()))
	 (sheet (cdr (assoc paper style-sheet-alist)))
	 (fontnm (qualifiers-to-fontname qs sheet))
	 (err (current-error-port)))
    (if (eq? fontnm "")
	(begin
	  (display "\ncouldn't find font satisfying " err)
	  (display qs err)
	  (display "\n" err)	  
	  "cmr10")
	fontnm)))

			    


; (define (test-module)
;  (display (filter-list pair? '(1 2 (1 2) (1 .2)))
;(display (filter-field 'font-name 'cmbx paper20-style-sheet-alist))

; (display (qualifiers-to-fontname '((font-name . cmbx)) paper20-style-sheet-alist))
; (display (style-to-font-name 'paper20 'large))
; )


