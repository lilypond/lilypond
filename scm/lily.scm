;;; lily.scm -- implement Scheme output routines for TeX and PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  1998--2003 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;; Library functions


(use-modules (ice-9 regex))


;;; General settings
;; debugging evaluator is slower.

(debug-enable 'debug)
;(debug-enable 'backtrace)
(read-enable 'positions)


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

;;;;;;;;;;;;;;;;
; list
(define (tail lst)
  "Return tail element of LST."
  (car (last-pair lst)))


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
  (if (pair? a)
      (if (pair? b)
	  (if (member (car a) b)
	      (list-minus (cdr a) b)
	      (cons (car a) (list-minus (cdr a) b)))
	  a)
      '()))

;; why -list suffix (see reduce-list)
(define-public (filter-list pred? list)
  "return that part of LIST for which PRED is true.

 TODO: rewrite using accumulator. Now it takes O(n) stack. "
  
  (if (null? list) '()
      (let* ((rest (filter-list pred? (cdr list))))
	(if (pred? (car list))
	    (cons (car list)  rest)
	    rest))))

(define-public (filter-out-list pred? list)
  "return that part of LIST for which PRED is false."
  (if (null? list) '()
      (let* ((rest (filter-out-list pred? (cdr list))))
	(if (not (pred? (car list)))
	    (cons (car list)  rest)
	    rest))))


(define (first-n n lst)
  "Return first N elements of LST"
  (if (and (pair? lst)
	   (> n 0))
      (cons (car lst) (first-n (- n 1) (cdr lst)))
      '()))

(define-public (uniq-list list)
  (if (null? list) '()
      (if (null? (cdr list))
	  list
	  (if (equal? (car list) (cadr list))
	      (uniq-list (cdr list))
	      (cons (car list) (uniq-list (cdr list)))))))

(define (butfirst-n n lst)
  "Return all but first N entries of LST"
  (if (pair? lst)
      (if (> n 0)
	  (butfirst-n (- n 1) (cdr lst))
	  lst)
      '()))
  
(define (split-at predicate l)
 "Split L = (a_1 a_2 ... a_k b_1 ... b_k)
into L1 = (a_1 ... a_k ) and L2 =(b_1 .. b_k) 
Such that (PREDICATE a_i a_{i+1}) and not (PREDICATE a_k b_1).
L1 is copied, L2 not.

(split-at (lambda (x y) (= (- y x) 2))  '(1 3 5 9 11) (cons '() '()))"
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

(define (split-one sep?  l acc)
  "Split off the first parts before separator and return both parts.

"
  ;; " KUT EMACS
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
      )
    )
)


(define (other-axis a)
  (remainder (+ a 1) 2))
  

(define-public (widen-interval iv amount)
   (cons (- (car iv) amount)
         (+ (cdr iv) amount))
)

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

;; used where?
(define-public (reduce operator list)
  "reduce OP [A, B, C, D, ... ] =
   A op (B op (C ... ))
"
      (if (null? (cdr list)) (car list)
	  (operator (car list) (reduce operator (cdr list)))))

(define (take-from-list-until todo gathered crit?)
  "return (G, T), where (reverse G) + T = GATHERED + TODO, and the last of G
is the  first to satisfy CRIT

 (take-from-list-until '(1 2 3  4 5) '() (lambda (x) (eq? x 3)))
=>
 ((3 2 1) 4 5)

"
  (if (null? todo)
      (cons gathered todo)
      (if (crit? (car todo))
	  (cons (cons (car todo) gathered) (cdr todo))
	  (take-from-list-until (cdr todo) (cons (car todo) gathered) crit?)
      )
  ))

(define-public (list-insert-separator list between)
  "Create new list, inserting BETWEEN between elements of LIST"
  (if (null? list)
      '()
      (if (null? (cdr list))
	  list
	  (cons (car list)
		(cons between (list-insert-separator (cdr list) between)))
  
  )))

;;;;;;;;;;;;;;;;
; strings.


;; TODO : make sep optional.
(define-public (string-join str-list sep)
  "append the list of strings in STR-LIST, joining them with SEP"
  
  (apply string-append (list-insert-separator str-list sep))
  )

(define-public (pad-string-to str wid)
  (string-append str (make-string (max (- wid (string-length str)) 0) #\ ))
  )

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
    (if (ly:verbose)
	(format (current-error-port) "[~A]" fn))
    (primitive-load fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  output
(use-modules (scm tex)
	     (scm ps)
	     (scm pysk)
	     (scm ascii-script)
	     (scm sketch)
	     (scm sodipodi)
	     (scm pdftex)
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
     '("music-types.scm"
       "output-lib.scm"
       "c++.scm"
       "chords-ignatzek.scm"
       "chord-entry.scm"
       "double-plus-new-chord-name.scm"
       "molecule.scm"
       "new-markup.scm"
       "bass-figure.scm"
       "music-functions.scm"
       "music-property-description.scm"
       "auto-beam.scm"
       "basic-properties.scm"
       "chord-name.scm"
       "translator-property-description.scm"
       "script.scm"
       "drums.scm"
       "midi.scm"

       "beam.scm"
       "clef.scm"
       "slur.scm"
       "font.scm"
       
       "grob-property-description.scm"
       "grob-description.scm"
       "context-description.scm"
       "interface-description.scm"
       ))


       


(set! type-p-name-alist
  `(
   (,ly:dir? . "direction")
   (,scheme? . "any type")
   (,number-pair? . "pair of numbers")
   (,ly:input-location? . "input location")   
   (,ly:grob? . "grob (GRaphical OBject)")
   (,grob-list? . "list of grobs")
   (,ly:duration? . "duration")
   (,pair? . "pair")
   (,integer? . "integer")
   (,list? . "list")
   (,symbol? . "symbol")
   (,string? . "string")
   (,boolean? . "boolean")
   (,ly:pitch? . "pitch")
   (,ly:moment? . "moment")
   (,ly:dimension? . "dimension, in staff space")
   (,ly:input-location? . "input location")
   (,music-list? . "list of music")
   (,ly:music? . "music")
   (,number? . "number")
   (,char? . "char")
   (,input-port? . "input port")
   (,output-port? . "output port")   
   (,vector? . "vector")
   (,procedure? . "procedure") 
   (,boolean-or-symbol? . "boolean or symbol")
   (,number-or-string? . "number or string")
   (,markup? . "markup")
   (,markup-list? . "list of markups")
   (,number-or-grob? . "number or grob")
   ))
