;;; lily.scm -- implement Scheme output routines for TeX and PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2003 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;; Library functions


(use-modules (ice-9 regex)
	     (srfi srfi-1)		;lists
	     (srfi srfi-13)		;strings
	     )

;;; General settings
;;; debugging evaluator is slower.  This should
;;; have a more sensible default.


(if (ly:get-option 'verbose)
    (begin
      (debug-enable 'debug)
      (debug-enable 'backtrace)
      (read-enable 'positions)))


(define-public (line-column-location line col file)
  "Print an input location, including column number ."
  (string-append (number->string line) ":"
		 (number->string col) " " file)
  )

(define-public (line-location line col file)
  "Print an input location, without column number ."
  (string-append (number->string line) " " file)
  )

(define-public point-and-click #f)

(define-public (lilypond-version)
  (string-join
   (map (lambda (x) (if (symbol? x)
			(symbol->string x)
			(number->string x)))
		(ly:version))
   "."))



;; cpp hack to get useful error message
(define ifdef "First run this through cpp.")
(define ifndef "First run this through cpp.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public X 0)
(define-public Y 1)
(define-public START -1)
(define-public STOP 1)
(define-public LEFT -1)
(define-public RIGHT 1)
(define-public UP 1)
(define-public DOWN -1)
(define-public CENTER 0)

(define-public DOUBLE-FLAT -4)
(define-public THREE-Q-FLAT -3)
(define-public FLAT -2)
(define-public SEMI-FLAT -1)
(define-public NATURAL 0)
(define-public SEMI-SHARP 1)
(define-public SHARP 2)
(define-public THREE-Q-SHARP 3)
(define-public DOUBLE-SHARP 4)
(define-public SEMI-TONE 2)

(define-public ZERO-MOMENT (ly:make-moment 0 1)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lily specific variables.
(define-public default-script-alist '())

(define-public security-paranoia #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unassorted utility functions.


;;;;;;;;;;;;;;;;
; alist
(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)))))


(define (assoc-get key alist)
  "Return value if KEY in ALIST, else #f."
  (let ((entry (assoc key alist)))
    (if entry (cdr entry) #f)))
  
(define (assoc-get-default key alist default)
  "Return value if KEY in ALIST, else DEFAULT."
  (let ((entry (assoc key alist)))
    (if entry (cdr entry) default)))


(define-public (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)))))

(define-public (alist<? x y)
  (string<? (symbol->string (car x))
	    (symbol->string (car y))))



(define (chain-assoc x alist-list)
  (if (null? alist-list)
      #f
      (let* ((handle (assoc x (car alist-list))))
	(if (pair? handle)
	    handle
	    (chain-assoc x (cdr alist-list))))))

(define (chain-assoc-get x alist-list default)
  (if (null? alist-list)
      default
      (let* ((handle (assoc x (car alist-list))))
	(if (pair? handle)
	    (cdr handle)
	    (chain-assoc-get x (cdr alist-list) default)))))


(define (map-alist-vals func list)
  "map FUNC over the vals of  LIST, leaving the keys."
  (if (null?  list)
      '()
      (cons (cons  (caar list) (func (cdar list)))
	    (map-alist-vals func (cdr list)))
      ))

(define (map-alist-keys func list)
  "map FUNC over the keys of an alist LIST, leaving the vals. "
  (if (null?  list)
      '()
      (cons (cons (func (caar list)) (cdar list))
	    (map-alist-keys func (cdr list)))
      ))
 


;;;;;;;;;;;;;;;;
; list

(define (flatten-list lst)
  "Unnest LST" 
  (if (null? lst)
      '()
      (if (pair? (car lst))
	  (append (flatten-list (car lst)) (flatten-list  (cdr lst)))
	  (cons (car lst) (flatten-list (cdr lst))))
  ))

(define (list-minus a b)
  "Return list of elements in A that are not in B."
  (lset-difference eq? a b))


;; TODO: use the srfi-1 partition function.
(define-public (uniq-list list)
  "Uniq LIST, assuming that it is sorted"
  (if (null? list) '()
      (if (null? (cdr list))
	  list
	  (if (equal? (car list) (cadr list))
	      (uniq-list (cdr list))
	      (cons (car list) (uniq-list (cdr list)))))))

(define (split-at-predicate predicate l)
 "Split L = (a_1 a_2 ... a_k b_1 ... b_k)
into L1 = (a_1 ... a_k ) and L2 =(b_1 .. b_k) 
Such that (PREDICATE a_i a_{i+1}) and not (PREDICATE a_k b_1).
L1 is copied, L2 not.

(split-at-predicate (lambda (x y) (= (- y x) 2))  '(1 3 5 9 11) (cons '() '()))"
;; "

;; KUT EMACS MODE.

  (define (inner-split predicate l acc)
  (cond
   ((null? l) acc)
   ((null? (cdr l))
    (set-car! acc (cons (car l) (car acc)))
    acc)
   ((predicate (car l) (cadr l))
    (set-car! acc (cons (car l) (car acc)))
    (inner-split predicate (cdr l) acc))
   (else
    (set-car! acc (cons (car l) (car acc)))
    (set-cdr! acc (cdr l))
    acc)

  ))
 (let*
    ((c (cons '() '()))
     )
  (inner-split predicate l  c)
  (set-car! c (reverse! (car c))) 
  c)
)


(define-public (split-list l sep?)
"
(display (split-list '(a b c / d e f / g) (lambda (x) (equal? x '/))) )
=>
((a b c) (d e f) (g))

"
;; " KUT EMACS.

(define (split-one sep?  l acc)
  "Split off the first parts before separator and return both parts."
  (if (null? l)
      (cons acc '())
      (if (sep? (car l))
	  (cons acc (cdr l))
	  (split-one sep? (cdr l) (cons (car l) acc))
	  )
      ))

(if (null? l)
    '()
    (let* ((c (split-one sep? l '())))
      (cons (reverse! (car c) '()) (split-list (cdr c) sep?))
      )))


(define-public (interval-length x)
  "Length of the number-pair X, when an interval"
  (max 0 (- (cdr x) (car x)))
  )
  

(define (other-axis a)
  (remainder (+ a 1) 2))
  

(define-public (interval-widen iv amount)
   (cons (- (car iv) amount)
         (+ (cdr iv) amount)))

(define-public (interval-union i1 i2)
   (cons (min (car i1) (car i2))
	 (max (cdr i1) (cdr i2))))


(define-public (write-me message x)
  "Return X.  Display MESSAGE and write X.  Handy for debugging, possibly turned off."
  (display message) (write x) (newline) x)
;;  x)

(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

(define (cons-map f x)
  "map F to contents of X"
  (cons (f (car x)) (f (cdr x))))


(define-public (list-insert-separator lst between)
  "Create new list, inserting BETWEEN between elements of LIST"
  (define (conc x y )
    (if (eq? y #f)
	(list x)
	(cons x  (cons between y))
	))
  (fold-right conc #f lst))

;;;;;;;;;;;;;;;;
; other
(define (sign x)
  (if (= x 0)
      0
      (if (< x 0) -1 1)))

(define-public (!= l r)
  (not (= l r)))

(define-public (ly:load x)
  (let* (
	 (fn (%search-load-path x))

	 )
    (if (ly:get-option 'verbose)
	(format (current-error-port) "[~A]" fn))
    (primitive-load fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  output
(use-modules (scm output-tex)
	     (scm output-ps)
	     (scm output-ascii-script)
	     (scm output-sketch)
	     (scm output-sodipodi)
	     (scm output-pdftex)
	     )

(define output-alist
  `(
    ("tex" . ("TeX output. The default output form." ,tex-output-expression))
    ("ps" . ("Direct postscript. Requires setting GS_LIB and GS_FONTPATH" ,ps-output-expression))
    ("scm" . ("Scheme dump: debug scheme molecule expressions" ,write))
    ("as" . ("Asci-script. Postprocess with as2txt to get ascii art"  ,as-output-expression))
    ("sketch" . ("Bare bones Sketch output." ,sketch-output-expression))
    ("sodipodi" . ("Bare bones Sodipodi output." ,sodipodi-output-expression))
    ("pdftex" . ("PDFTeX output. Was last seen nonfunctioning." ,pdftex-output-expression))
    ))


(define (document-format-dumpers)
  (map
   (lambda (x)
     (display (string-append  (pad-string-to 5 (car x)) (cadr x) "\n"))
     output-alist)
   ))

(define-public (find-dumper format )
  (let*
      ((d (assoc format output-alist)))
    
    (if (pair? d)
	(caddr d)
	(scm-error "Could not find dumper for format ~s" format))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other files.

(map ly:load
					; load-from-path
     '("define-music-types.scm"
       "output-lib.scm"
       "c++.scm"
       "chord-ignatzek-names.scm"
       "chord-entry.scm"
       "chord-generic-names.scm"
       "molecule.scm"
       "new-markup.scm"
       "bass-figure.scm"
       "music-functions.scm"
       "define-music-properties.scm"
       "auto-beam.scm"
       "chord-name.scm"
       
       "define-translator-properties.scm"
       "translation-functions.scm"
       "script.scm"
       "drums.scm"
       "midi.scm"

       "beam.scm"
       "clef.scm"
       "slur.scm"
       "font.scm"
       
       "define-grob-properties.scm"
       "define-grobs.scm"
       "define-grob-interfaces.scm"

       "paper.scm"
       ))


       


(set! type-p-name-alist
  `(
   (,boolean-or-symbol? . "boolean or symbol")
   (,boolean? . "boolean")
   (,char? . "char")
   (,grob-list? . "list of grobs")
   (,input-port? . "input port")
   (,integer? . "integer")
   (,list? . "list")
   (,ly:context? . "context")
   (,ly:dimension? . "dimension, in staff space")
   (,ly:dir? . "direction")
   (,ly:duration? . "duration")
   (,ly:grob? . "grob (GRaphical OBject)")
   (,ly:input-location? . "input location")
   (,ly:input-location? . "input location")   
   (,ly:moment? . "moment")
   (,ly:music? . "music")
   (,ly:pitch? . "pitch")
   (,ly:translator? . "translator")
   (,markup-list? . "list of markups")
   (,markup? . "markup")
   (,music-list? . "list of music")
   (,number-or-grob? . "number or grob")
   (,number-or-string? . "number or string")
   (,number-pair? . "pair of numbers")
   (,number? . "number")
   (,output-port? . "output port")   
   (,pair? . "pair")
   (,procedure? . "procedure") 
   (,scheme? . "any type")
   (,string? . "string")
   (,symbol? . "symbol")
   (,vector? . "vector")
   ))
