;;;; lily.scm -- implement Scheme output routines for TeX and PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;; Library functions


(if (defined? 'set-debug-cell-accesses!)
    (set-debug-cell-accesses! #f))

;(set-debug-cell-accesses! 5000)

(use-modules (ice-9 regex)
	     (ice-9 safe)
	     (oop goops)
	     (srfi srfi-1)  ; lists
	     (srfi srfi-13)) ; strings


; my display

(define-public (myd k v) (display k) (display ": ") (display v) (display ", "))

(define-public (print . args)
  (apply format (cons (current-output-port) args)))
  

;;; General settings
;;; debugging evaluator is slower.  This should
;;; have a more sensible default.

(if (ly:get-option 'verbose)
    (begin
      (debug-enable 'debug)
      (debug-enable 'backtrace)
      (read-enable 'positions)))

(define-public (line-column-location file line col)
  "Print an input location, including column number ."
  (string-append (number->string line) ":"
		 (number->string col) " " file))

(define-public (line-location  file line col)
  "Print an input location, without column number ."
  (string-append (number->string line) " " file))

(define-public point-and-click #f)

(define-public parser #f)

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

;; gettext wrapper for guile < 1.7.2
(if (defined? 'gettext)
    (define-public _ gettext)
    (define-public _ ly:gettext))

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

(define-public (moment-min a b)
  (if (ly:moment<? a b) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lily specific variables.

(define-public default-script-alist '())


;; parser stuff.
(define-public (print-music-as-book parser music)
  (let* ((head  (ly:parser-lookup parser '$globalheader))
	 (book (ly:make-book (ly:parser-lookup parser $defaultpaper)
			     head score)))
    (ly:parser-print-book parser book)))

(define-public (print-score-as-book parser score)
  (let*
      ((head  (ly:parser-lookup parser '$globalheader))
       (book (ly:make-book (ly:parser-lookup parser $defaultpaper)
			   head score)))
    (ly:parser-print-book parser book)))

(define-public (print-score parser score)
  (let* ((head  (ly:parser-lookup parser '$globalheader))
	 (book (ly:make-book (ly:parser-lookup parser $defaultpaper)
			     head score)))
    (ly:parser-print-score parser book)))
		
(define-public (collect-scores-for-book  parser score)
  (let*
      ((oldval (ly:parser-lookup parser 'toplevel-scores)))
    (ly:parser-define parser 'toplevel-scores (cons score oldval))
    ))

(define-public (collect-music-for-book parser music)
  (collect-scores-for-book parser (ly:music-scorify music parser)))


  
;;;;;;;;;;;;;;;;
; alist
(define-public assoc-get ly:assoc-get)

(define-public (uniqued-alist alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)))))

(define-public (alist<? x y)
  (string<? (symbol->string (car x))
	    (symbol->string (car y))))

(define-public (chain-assoc x alist-list)
  (if (null? alist-list)
      #f
      (let* ((handle (assoc x (car alist-list))))
	(if (pair? handle)
	    handle
	    (chain-assoc x (cdr alist-list))))))

(define-public (chain-assoc-get x alist-list . default)
  "Return ALIST entry for X. Return DEFAULT (optional, else #f) if not
found."

  (define (helper x alist-list default)
    (if (null? alist-list)
	default
	(let* ((handle (assoc x (car alist-list))))
	  (if (pair? handle)
	      (cdr handle)
	      (helper x (cdr alist-list) default)))))

  (helper x alist-list
	  (if (pair? default) (car default) #f)))

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
;; hash



(if (not (defined? 'hash-table?))	; guile 1.6 compat
    (begin
      (define hash-table? vector?)

      (define-public (hash-table->alist t)
	"Convert table t to list"
	(apply append
	       (vector->list t)
	       )))

    ;; native hashtabs.
    (begin
      (define-public (hash-table->alist t)

	(hash-fold (lambda (k v acc) (acons  k v  acc))
		   '() t)
	)
      ))

;; todo: code dup with C++. 
(define-public (alist->hash-table l)
  "Convert alist to table"
  (let
      ((m (make-hash-table (length l))))

    (map (lambda (k-v)
	   (hashq-set! m (car k-v) (cdr k-v)))
	 l)

    m))
       


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
(define-public (uniq-list l)
  
  "Uniq LIST, assuming that it is sorted"
  (define (helper acc l) 
    (if (null? l)
	acc
	(if (null? (cdr l))
	    (cons (car l) acc)
	    (if (equal? (car l) (cadr l))
		(helper acc (cdr l))
		(helper (cons (car l) acc)  (cdr l)))
	    )))
  (reverse! (helper '() l) '()))


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
(define-public interval-start car)
(define-public interval-end cdr)

(define (other-axis a)
  (remainder (+ a 1) 2))
  

(define-public (interval-widen iv amount)
   (cons (- (car iv) amount)
         (+ (cdr iv) amount)))

(define-public (interval-union i1 i2)
   (cons (min (car i1) (car i2))
	 (max (cdr i1) (cdr i2))))


(define-public (write-me message x)
  "Return X.  Display MESSAGE and write X.  Handy for debugging,
possibly turned off."
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

(define-public (symbol<? l r)
  (string<? (symbol->string l) (symbol->string r)))

(define-public (!= l r)
  (not (= l r)))

(define-public (ly:load x)
  (let* ((fn (%search-load-path x)))
    (if (ly:get-option 'verbose)
	(format (current-error-port) "[~A]" fn))
    (primitive-load fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  output

   
;;(define-public (output-framework) (write "hello\n"))

(define output-tex-module
  (make-module 1021 (list (resolve-interface '(scm output-tex)))))
(define output-ps-module
  (make-module 1021 (list (resolve-interface '(scm output-ps)))))

(define-public (ps-output-expression expr port)
  (display (eval expr output-ps-module) port))

;; TODO: generate this list by registering the stencil expressions
;;       stencil expressions should have docstrings.
(define-public (ly:all-stencil-expressions)
  "Return list of stencil expressions."
  '(
    beam
    bezier-sandwich
    blank
    bracket
    char
    dashed-line
    dashed-slur
    dot
    draw-line
    ez-ball
    filledbox
    horizontal-line
    polygon
    repeat-slash
    round-filled-box
    symmetric-x-triangle
    text
    tuplet
    white-dot
    white-text
    zigzag-line
    ))

;; TODO:
;;  - generate this list by registering the output-backend-commands
;;    output-backend-commands should have docstrings.
;;  - remove hard copies in output-ps output-tex
(define-public (ly:all-output-backend-commands)
  "Return list of output backend commands."
  '(
    comment
    grob-cause
    no-origin
    placebox
    unknown
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other files.

(for-each ly:load
     ;; load-from-path
     '("define-music-types.scm"
       "output-lib.scm"
       "c++.scm"
       "chord-ignatzek-names.scm"
       "chord-entry.scm"
       "chord-generic-names.scm"
       "stencil.scm"
       "new-markup.scm"
       "bass-figure.scm"
       "music-functions.scm"
       "part-combiner.scm"
       "define-music-properties.scm"
       "auto-beam.scm"
       "chord-name.scm"

       "ly-from-scheme.scm"
       
       "define-context-properties.scm"
       "translation-functions.scm"
       "script.scm"
       "midi.scm"
       "beam.scm"
       "clef.scm"
       "slur.scm"
       "font.scm"
       "encoding.scm"
       
       "fret-diagrams.scm"
       "define-markup-commands.scm"
       "define-grob-properties.scm"
       "define-grobs.scm"
       "define-grob-interfaces.scm"
       "page-layout.scm"
       "titling.scm"
       
       "paper.scm"

       ; last:
       "safe-lily.scm"
       ))


(set! type-p-name-alist
  `(
   (,boolean-or-symbol? . "boolean or symbol")
   (,boolean? . "boolean")
   (,char? . "char")
   (,grob-list? . "list of grobs")
   (,hash-table? . "hash table")
   (,input-port? . "input port")
   (,integer? . "integer")
   (,list? . "list")
   (,ly:context? . "context")
   (,ly:dimension? . "dimension, in staff space")
   (,ly:dir? . "direction")
   (,ly:duration? . "duration")
   (,ly:grob? . "layout object")
   (,ly:input-location? . "input location")
   (,ly:moment? . "moment")
   (,ly:music? . "music")
   (,ly:pitch? . "pitch")
   (,ly:translator? . "translator")
   (,ly:font-metric? . "font metric")
   (,markup-list? . "list of markups")
   (,markup? . "markup")
   (,ly:music-list? . "list of music")
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


;; debug mem leaks

(define gc-protect-stat-count 0)
(define-public (dump-gc-protects)
  (set! gc-protect-stat-count (1+ gc-protect-stat-count) )
  (let*
      ((protects (sort
	   (hash-table->alist (ly:protects))
	   (lambda (a b)
	     (< (object-address (car a))
		(object-address (car b))))))
       (outfile    (open-file (string-append
	       "gcstat-" (number->string gc-protect-stat-count)
	       ".scm"
	       ) "w")))

    (display "DUMPING...\n")
    (display
     (filter
      (lambda (x) (not (symbol? x))) 
      (map (lambda (y)
	     (let
		 ((x (car y))
		  (c (cdr y)))

	       (string-append
		(string-join
		 (map object->string (list (object-address x) c x))
		 " ")
		"\n")))
	   protects))
     outfile)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define-public (ly:system command)
  (let* ((status 0))
    (if (ly:get-option 'verbose)
	(format  (current-error-port) (_ "Invoking `~a'...\n") command))
    (set! status (system command))
    (if (> status 0)
	(format (current-error-port) (_ "Error invoking `~a'. Return value ~a")
			 command status))))

(define-public (postscript->pdf papersizename name)
  (let* ((cmd (string-append "ps2pdf -sPAPERSIZE=" papersizename " " name))
	 (output-name
	  (regexp-substitute/global #f "\\.ps" name 'pre ".pdf" 'post)))
    (format (current-error-port) (_ "Converting to `~a'...") output-name)
    (ly:system cmd)))

(define-public (postscript->png resolution name)
  (let
      ((cmd (string-append
	   "ps2png --resolution="
	   (if (number? resolution)
	       (number->string resolution)
	       "90 ")
	   (if (ly:get-option 'verbose)
	       "--verbose "
	       " ")
	   name)))
    (ly:system cmd)))

(define-public (lilypond-main files)
  "Entry point for LilyPond."
  (let* ((failed '())
	 (handler (lambda (key arg) (set! failed (cons arg failed)))))
    (for-each
     (lambda (f)
       (catch 'ly-file-failed (lambda () (ly:parse-file f)) handler)
;;;       (dump-gc-protects)
       )
     files)

    (if (pair? failed)
	(begin
	  (newline (current-error-port))
	  (display (_ "error: failed files: ") (current-error-port))
	  (display (string-join failed) (current-error-port))
	  (newline (current-error-port))
	  (newline (current-error-port))
	  (exit 1))
	(exit 0))))

