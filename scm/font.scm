;;;
;;; font.scm -- implement Font stuff
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
;;;

(define style-to-font-alist
  `(
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
	((eq? name 'font-point-size) 5)
	(else (ly-warning "unknown font field name"))
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

;; should really have name/pt size at the front of the list.
;;
;; (also tried to vary the order of this list, with little effect)
(define paper20-style-sheet-alist
  '(
    ((3 medium upright number feta-nummer 13) . "feta-nummer13")
    ((2 medium upright number feta-nummer 13) . "feta-nummer13")
    ((1 medium upright number feta-nummer 11) . "feta-nummer11")
    ((0 medium upright number feta-nummer 10) . "feta-nummer10")
    ((-1 medium upright number feta-nummer 8) . "feta-nummer8")
    ((-2 medium upright number feta-nummer 7) . "feta-nummer7")
    ((-3 medium upright number feta-nummer 6) . "feta-nummer6")
    ((-4 medium upright number feta-nummer 5) . "feta-nummer5")
    ((-5 medium upright number feta-nummer 4) . "feta-nummer4")

    ((4 medium upright roman cmr 20) . "cmr20")
    ((3 medium upright roman cmr 16) . "cmr16")
    ((2 medium upright roman cmr 14) . "cmr14")
    ((1 medium upright roman cmr 12) . "cmr12")
    ((0 medium upright roman cmr 10) . "cmr10")
    ((-1 medium upright roman cmr 8) . "cmr8" )
    ((-2 medium upright roman cmr 7) . "cmr7" )
    ((-3 medium upright roman cmr 6) . "cmr6" )
    ((-4 medium upright roman cmr 5) . "cmr5" )
    ((-5 medium upright roman cmr 4) . "cmr4" )

    ((3 medium italic roman cmti 16) . "cmti16")
    ((2 medium italic roman cmti 14) . "cmti14")
    ((1 medium italic roman cmti 12) . "cmti12")
    ((0 medium italic roman cmti 10) . "cmti10")
    ((-1 medium italic roman cmti 8) . "cmti8")    
    ((-2 medium italic roman cmti 7) . "cmti7")
    ((-3 medium italic roman cmti 6) . "cmti6")    

    ((2 bold upright roman cmbx 14) . "cmbx14")
    ((1 bold upright roman cmbx 12) . "cmbx12")
    ((0 bold upright roman cmbx 10) . "cmbx10")
    ((-1 bold upright roman cmbx 8) . "cmbx8")
    ((-2 bold upright roman cmbx 7) . "cmbx7")
    
    ;; should use the same brace font every where and fix C++ code.
    ((2 medium upright braces feta-braces 26) . "feta-braces26")
    ((1 medium upright braces feta-braces 23) . "feta-braces23")
    ((0 medium upright braces feta-braces 20) . "feta-braces20")
    ((-1 medium upright braces feta-braces 16) . "feta-braces16")
    ((-2 medium upright braces feta-braces 13) . "feta-braces13")
    ((-3 medium upright braces feta-braces 11) . "feta-braces11")

    ((3 bold italic dynamic feta-din 13) . "feta-din13")
    ((2 bold italic dynamic feta-din 13) . "feta-din13")
    ((1 bold italic dynamic feta-din 11) . "feta-din11")
    ((0 bold italic dynamic feta-din 10) . "feta-din10")
    ((-1 bold italic dynamic feta-din 8) . "feta-din8")
    ((-2 bold italic dynamic feta-din 7) . "feta-din7")
    ((-3 bold italic dynamic feta-din 6) . "feta-din6")
    ((-4 bold italic dynamic feta-din 5) . "feta-din5")
    ((-5 bold italic dynamic feta-din 4) . "feta-din4")

    ((2 medium upright music feta 26) . "feta26")
    ((1 medium upright music feta 23) . "feta23")
    ((0 medium upright music feta 20) . "feta20")
    ((-0.5 medium upright music feta 20) . "feta19")    
    ((-1 medium upright music feta 16) . "feta16")
    ((-2 medium upright music feta 13) . "feta13")
    ((-3 medium upright music feta 11) . "feta11")
    ((-4 medium upright music feta 11) . "feta11")

    ((0 medium upright math msam 10) . "msam10")
    ((-1 medium upright math msam 10) . "msam10")
    ((-2 medium upright math msam 10) . "msam10")
    ((-3 medium upright math msam 10) . "msam10")
   ))

;; return a FONT-DESCR with relative size decremented by DECREMENT
(define (change-relative-size font-desc decrement)
  (cons (- (car font-desc) decrement) (cdr font-desc))
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
(define font-list-alist
  (map-alist-vals (lambda (x) (change-style-sheet-relative-size
			       paper20-style-sheet-alist x))
		  '((paper11 . -3)
		    (paper13 . -2)
		    (paper16 . -1)
		    (paper19 . -0.5)	
		    (paper20 . 0)
		    (paper23 . 1)
		    (paper26 . 2)
		    ))
  )


(define (make-style-sheet sym)
  `((fonts . ,(cdr (assoc sym font-list-alist)))
    (font-defaults
     . ((font-family . music)
	(font-relative-size . 0)
	(font-shape . upright)
	(font-series . medium)
	))
    (style-alist
     . ((finger . ((font-family . number) (font-relative-size . -3)))
	(volta . ((font-family . number) (font-relative-size . -2)))
	(tuplet . ((font-family . roman) (font-shape . italic) (font-relative-size . -1)))

	(timesig . ((font-family . number) (font-relative-size . 0)))
	(timesig-symbol . ((font-family . music) (font-relative-size . 0)))
	
	(mmrest . ((font-family . number) (font-relative-size . 1)))
	(mmrest-symbol . ((font-family . music) (font-relative-size . 0)))

	(mark . ((font-family . number) (font-relative-size . 1)))
	(script . ((font-family . roman) (font-relative-size . -1)))
	(large . ((font-family . roman) (font-relative-size . 1)))
	(Large . ((font-series . bold) (font-family . roman) (font-relative-size . 2)))
	(dynamic . ((font-series . bold) (font-family . dynamic) (font-relative-size . 0)))
	))
    (properties-to-font .

			,Font_interface::properties_to_font_name)
			; ,properties-to-font-name)

    ;; FIXME: this is a not-so-cool idea to use ALIGN
    ;; RAISE, LOOKUP, since they are not proper elt-properties,
    ;; and might interfere with them.
    (markup-to-properties . ,markup-to-properties)
    (abbreviation-alist
     . ((rows . ((align . 0)))
	(lines . ((align . 1)))
	(roman . ((font-family . roman)))
	(music . ((font-family . music) (font-shape . upright)))
	(finger . ((font-style . finger)))
	(bold . ((font-series . bold)))
	(italic . ((font-shape . italic)))
	(named . ((lookup . name)))
	(super . ((raise . 1) (font-relative-size . -1)))
	(sub . ((raise . -1) (font-relative-size . -1)))
	(text . ((lookup . value)))
	)
     )
    
    )
  )

;; reduce the font list by successively applying a font-qualifier.
(define (qualifiers-to-fontnames  qualifiers font-descr-alist)
  (if (null? qualifiers)
      font-descr-alist
      
      (qualifiers-to-fontnames
       (cdr qualifiers)
       (filter-field (caar qualifiers) (cdar qualifiers) font-descr-alist)
      )
  ))


;; does FONT-DESC satisfy QUALIFIERS?
(define (font-qualifies? qualifiers font-desc)
  (if (null? qualifiers) #t
      (if (eq? (font-field (caar qualifiers) font-desc) (cdar qualifiers))
	  (font-qualifies? (cdr qualifiers) font-desc)
	  #f
	  )
	)
  )

(define (find-first-font qualifiers fonts)
  (if (null? fonts)
      ""
      (if (font-qualifies? qualifiers (caar fonts))
	  (cdar fonts)
	  (find-first-font qualifiers (cdr fonts))
	)
      ))


;; return a single font from FONTS (or a default, if none found)
;; and warn if the selected font is not unique.
(define (select-unique-font qualifiers fonts)
  (let*  (
	  (err (current-error-port))
	  )
    

  (if (not (= (length fonts) 1))
      (begin
	(display "\ncouldn't find unique font satisfying " err)
	(write qualifiers err)
	(display " found " err)
	  (if (null? fonts)
	      (display "none" err)
	      (write (map cdr  fonts) err))
	  ))

  (if (null? fonts)
      "cmr10"
      (cdar fonts))	; return the topmost.
  
  ))


(define (chain-assoc x alist-list)
  (if (null? alist-list)
      #f
      (let* (
	     (handle (assoc x (car alist-list)))
	     )
	(if (pair? handle)
	    handle
	    (chain-assoc x (cdr alist-list))
	    )
	)
      )
  )

;; TODO
;; add support for override by font-name
;; very often-used; hard-code in C++, and use SCM glue code.

(define (properties-to-font-name fonts properties-alist-list)
  (let*  (
	  ;; change order to change priorities of qualifiers.
	  (q-order    '(font-name font-family font-series font-shape font-point-size font-relative-size))
	  (rawqualifiers (map (lambda (x) (chain-assoc x  properties-alist-list))
			      q-order))
	  (qualifiers (filter-list pair? rawqualifiers))
	  (selected     (find-first-font qualifiers fonts))
	  (err (current-error-port))	  
	  )

    (if (equal? selected "")
	(begin
	  (display "\ncouldn't find any font satisfying " err)
	  (write qualifiers err)
	  "cmr10"
	  )
	selected)	; return the topmost.
    ))

(define (markup-to-properties sheet markup)
  ;;(display "markup: `")
  ;;(write markup)
  ;;(display "'\n")
  (if (pair? markup)
      (list markup)
      (let ((entry (assoc markup
			  ;; assoc-chain?
			  (append (cdr (assoc 'abbreviation-alist sheet))
				  (cdr (assoc 'style-alist sheet))))))
	(if entry
	    (cdr entry)
	    (list (cons markup #t))))))

; fixme, how's this supposed to work?
; and why don't we import font-setting from elt?
(define (style-to-font-name sheet style)
  (let* ((entry (assoc style style-to-font-alist))
	 (qualifiers (if entry (cdr entry) '()))
	 (font (find-first-font qualifiers sheet))
	 (err (current-error-port))
	 )

    (if (equal? font "")
	(begin
	  (display "\ncouldn't find any font satisfying " err)
	  (write qualifiers err)
	  "cmr10"
	  )
	font)	; return the topmost.
    ))

		    


; (define (test-module)
;  (display (filter-list pair? '(1 2 (1 2) (1 .2)))
;(display (filter-field 'font-name 'cmbx paper20-style-sheet-alist))

; (display (qualifiers-to-fontname '((font-name . cmbx)) paper20-style-sheet-alist))
; (display (style-to-font-name 'paper20 'large))
; )


