;;;
;;; font.scm -- implement Font stuff
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
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
	((eq? name 'font-design-size) 4)
	(else (ly-warning "unknown font field name"))
	)
       ))



;; most of these routines have been reimplemented in C++ 

;; TODO TODO . (should not use filtering?)
;; this is bad, since we generate garbage every font-lookup.
;; otoh, if the qualifiers is narrow enough , we don't generate much garbage.

(define (filter-field field-name value font-descr-alist)
  "return those descriptions from FONT-DESCR-LIST whose FIELD-NAME matches VALUE"
      (filter-list
       (lambda (x) (let* (field-value (font-field field-name (car x))) 
		     (or (eq? field-value '*) (eq? value field-value))))
       font-descr-alist)
      )

(define paper-style-sheet-alist
  '(
    ((8 * * braces 8) . "feta-braces8")
    ((7 * * braces 7) . "feta-braces7")
    ((6 * * braces 6) . "feta-braces6")
    ((5 * * braces 5) . "feta-braces5")
    ((4 * * braces 4) . "feta-braces4")
    ((3 * * braces 3) . "feta-braces3")
    ((2 * * braces 2) . "feta-braces2")
    ((1 * * braces 1) . "feta-braces1")
    ((0 * * braces 0) . "feta-braces0")
    ))

;; FIXME: what about this comment?:
;;   should really have name/pt size at the front of the list.
;;   (also tried to vary the order of this list, with little effect)
;;
;; (font-relative-size font-series font-shape font-family 
;; font-design-size)
(define paper20-style-sheet-alist
  '(
    ;; why are font file names strings, not symbols?
    ((3 medium upright number 13) . "feta-nummer13")
    ((2 medium upright number 13) . "feta-nummer13")
    ((1 medium upright number 11) . "feta-nummer11")
    ((0 medium upright number 10) . "feta-nummer10")
    ((-1 medium upright number 8) . "feta-nummer8")
    ((-2 medium upright number 7) . "feta-nummer7")
    ((-3 medium upright number 6) . "feta-nummer6")
    ((-4 medium upright number 5) . "feta-nummer5")
    ((-5 medium upright number 4) . "feta-nummer4")

    ((4 medium upright roman 17) . "cmr17")
    ((3 medium upright roman 17) . "cmr17")
    ((2 medium upright roman 12) . "cmr12")
    ((1 medium upright roman 12) . "cmr12")
    ((0 medium upright roman 10) . "cmr10")
    ((-1 medium upright roman 8) . "cmr8" )
    ((-2 medium upright roman 7) . "cmr7" )
    ((-3 medium upright roman 6) . "cmr6" )
    ((-4 medium upright roman 5) . "cmr5" )
    ((-5 medium upright roman 5) . "cmr5" )

    ((3 medium italic roman 12) . "cmti12")
    ((2 medium italic roman 12) . "cmti12")
    ((1 medium italic roman 12) . "cmti12") ;;; ugh. Should add magnification here! 
    ((0 medium italic roman 10) . "cmti12")
    ((-1 medium italic roman 8) . "cmti10")    
    ((-2 medium italic roman 7) . "cmti8")
    ((-3 medium italic roman 7) . "cmti7")    

    ;;; cmbx17 is sauter, not commonly available as Type1.
    ((4 bold upright roman 17) . "cmr17") 
    ((3 bold upright roman 17) . "cmr17")
    
    ((2 bold upright roman 14) . "cmbx14")
    ((1 bold upright roman 12) . "cmbx12")
    ((0 bold upright roman 10) . "cmbx10")
    ((-1 bold upright roman 8) . "cmbx8")
    ((-2 bold upright roman 7) . "cmbx7")
    
    ((2 bold italic roman 12) . "cmbxti12")
    ((1 bold italic roman 12) . "cmbxti12")
    ((0 bold italic roman 10) . "cmbxti10")

    ((-1 bold italic roman 8) . "cmbxti8")
    ((-2 bold italic roman 7) . "cmbxti7")
    
    ((4 medium upright typewriter 17) . "cmtt17")
    ((3 medium upright typewriter 17) . "cmtt17")
    ((2 medium upright typewriter 12) . "cmtt12")
    ((1 medium upright typewriter 12) . "cmtt12")
    ((0 medium upright typewriter 10) . "cmtt10")
    ((-1 medium upright typewriter 8) . "cmtt8" )
    ((-2 medium upright typewriter 7) . "cmtt7" )
    ((-3 medium upright typewriter 6) . "cmtt6" )
    ((-4 medium upright typewriter 5) . "cmtt5" )
    ((-5 medium upright typewriter 5) . "cmtt5" )
    
    ((3 medium caps roman 12) . "cmcsc12")
    ((2 medium caps roman 12) . "cmcsc12")
    ((1 medium caps roman 12) . "cmcsc12")
    ((0 medium caps roman 10) . "cmcsc10")
    ((-1 medium caps roman 8) . "cmcsc8")
    ((-2 medium caps roman 7) . "cmcsc7")
    ((-3 medium caps roman 7) . "cmcsc7")

    ((3 * * dynamic 19) . "feta-din19")
    ((2 * * dynamic 19) . "feta-din19")
    ((1 * * dynamic 17) . "feta-din17")
    ((0 * * dynamic 14) . "feta-din14")
    ((-1 * * dynamic 12) . "feta-din12")
    ((-2 * * dynamic 9) . "feta-din9")
    ((-3 * * dynamic 8) . "feta-din8")
    ((-4 * * dynamic 7) . "feta-din7")
    ((-5 * * dynamic 6) . "feta-din6")

    ((2 * * music 26) . "feta26")
    ((1 * * music 23) . "feta23")
    ((0 * * music 20) . "feta20")
    ((-0.5 * * music 20) . "feta19")    
    ((-1 * * music 16) . "feta16")
    ((-2 * * music 13) . "feta13")
    ((-3 * * music 11) . "feta11")
    ((-4 * * music 11) . "feta11")

    ((2 * * ancient 26) . "parmesan26")
    ((1 * * ancient 23) . "parmesan23")
    ((0 * * ancient 20) . "parmesan20")
    ((-0.5 * * ancient 20) . "parmesan19")    
    ((-1 * * ancient 16) . "parmesan16")
    ((-2 * * ancient 13) . "parmesan13")
    ((-3 * * ancient 11) . "parmesan11")
    ((-4 * * ancient 11) . "parmesan11")

    ((0 * * math 10) . "msam10")
    ((-1 * * math 10) . "msam10")
    ((-2 * * math 10) . "msam10")
    ((-3 * * math 10) . "msam10")
   ))

;; 
(define (change-relative-size font-desc decrement)
  "return a FONT-DESCR with relative size decremented by DECREMENT"
  (cons (- (car font-desc) decrement) (cdr font-desc))
  )

;; 
(define (map-alist-keys func list)
  "map a  function FUNC over the keys of an alist LIST, leaving the vals. "
  (if (null?  list)
      '()
      (cons (cons (func (caar list)) (cdar list))
	    (map-alist-keys func (cdr list)))
      ))
 

;; 
(define (map-alist-vals func list)
  "map a function FUNC over the vals of  LIST, leaving the keys."
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
  `((fonts . ,(append paper-style-sheet-alist
		      (cdr (assoc sym font-list-alist))))
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

	(timesig . ((font-family . number) ))
	(timesig-symbol . ((font-family . music) ))
	
	(mmrest . ((font-family . number) ))
	(mmrest-symbol . ((font-family . music) ))

	(mark-number . ((font-family . number) (font-relative-size . 1)))
	(mark-letter . ((font-family . roman)
			(font-series . bold)
			(font-shape . upright)
			(font-relative-size . 2)))
	
	(script . ((font-family . roman) (font-relative-size . -1)))
	(large . ((font-family . roman) (font-relative-size . 1)))
	(Large . ((font-series . bold) (font-family . roman)
		  (font-relative-size . 2)))
	(dynamic . ((font-family . dynamic) (font-relative-size . 0)))
	))
    (properties-to-font .
			,Font_interface::properties_to_font_name)

    (markup-to-properties . ,markup-to-properties)
    (abbreviation-alist
     . ((columns . ((axis . 0)))
	(lines . ((axis . 1)))
	(roman . ((font-family . roman)))
	(music . ((font-family . music) (lookup . name)))
	(finger . ((font-style . finger)))
	(bold . ((font-series . bold)))
	(upright . ((font-shape . upright)))
	(italic . ((font-shape . italic)))
	(named . ((lookup . name)))
	(overstrike . ((extent . (0 . 0))))
	(super . ((raise . 1) (font-relative-size . -1) (extent . (0 . 0))))
	(sub . ((raise . -1) (font-relative-size . -1) (extent . (0 . 0))))
	(text . ((lookup . value)))
	)
     )
    
    )
  )

(define (qualifiers-to-fontnames  qualifiers font-descr-alist)
  " reduce the font list by successively applying a font-qualifier."
  (if (null? qualifiers)
      font-descr-alist
      
      (qualifiers-to-fontnames
       (cdr qualifiers)
       (filter-field (caar qualifiers) (cdar qualifiers) font-descr-alist)
      )
  ))

(define (wild-eq? x y)
  (or (eq? x y)
      (eq? x '*)
      (eq? y '*)))
       
(define (font-qualifies? qualifiers font-desc)
  "does FONT-DESC satisfy QUALIFIERS?"
  (if (null? qualifiers) #t
      (if (wild-eq? (font-field (caar qualifiers) font-desc) (cdar qualifiers))
	  (font-qualifies? (cdr qualifiers) font-desc)
	  #f)))

(define (find-first-font qualifiers fonts)
  (if (null? fonts)
      ""
      (if (font-qualifies? qualifiers (caar fonts))
	  (cdar fonts)
	  (find-first-font qualifiers (cdr fonts))
	)
      ))


(define (select-unique-font qualifiers fonts)
  "return a single font from FONTS (or a default, if none found)
and warn if the selected font is not unique.
"
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
      (let* ((handle (assoc x (car alist-list))))
	(if (pair? handle)
	    handle
	    (chain-assoc x (cdr alist-list))))))

;; TODO
;; the C++ version  in font-interface.cc is usually used.
;;
;; FIXME: this has silently been broken by the introduction
;;        of wildcards in the font list.    
(define (properties-to-font-name fonts properties-alist-list)
  (let*  (
	  ;; change order to change priorities of qualifiers.
	  (q-order '(font-family font-series font-shape
			       font-design-size font-relative-size))
	  (rawqualifiers (map (lambda (x)
				(chain-assoc x properties-alist-list))
			      q-order))
	  (qualifiers (filter-list pair? rawqualifiers))
	  (selected (find-first-font qualifiers fonts))
	  (err (current-error-port)))

    (if (equal? selected "")
	(begin
	  (display "\ncouldn't find any font satisfying " err)
	  (write qualifiers err)
	  "cmr10"
	  )
	selected)	; return the topmost.
    ))

(define (markup-to-properties sheet markup)
  ;; (display "markup: `")
  ;; (write markup)
  ;; (display "'\n")
  
  (if (pair? markup)
      ;; This is hairy.  We want to allow:
      ;;    ((foo bar) "text")
      ;;    ((foo (bar . 1)) "text")
      ;;    ((foo . (0 . 1))) 
      
      (if (and (symbol? (car markup))
	       (or (not (pair? (cdr markup)))
		   (number? (cadr markup))))
	  (if (equal? '() (cdr markup))
	      (markup-to-properties sheet (car markup))
	      (list markup))
	  
	  (if (equal? '() (cdr markup))
	      (markup-to-properties sheet (car markup))
	      (append (markup-to-properties sheet (car markup))
		      (markup-to-properties sheet (cdr markup)))))
      
      ;; markup is single abbreviation
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

(if #f (begin
	 (define (test-module)
	   (display (filter-list pair? '(1 2 (1 2) (1 .2)))
		    (display (filter-field 'font-name 'cmbx paper20-style-sheet-alist))
		    
		    (display (qualifiers-to-fontname '((font-name . cmbx)) paper20-style-sheet-alist))
		    (display (style-to-font-name 'paper20 'large))
		    )
	   )
	 )


)
