;;;; lily.scm -- implement Scheme output routines for TeX and PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
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

;; cpp hack to get useful error message
(define ifdef "First run this through cpp.")
(define ifndef "First run this through cpp.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define X 0)
(define Y 1)
(define LEFT -1)
(define RIGHT 1)
(define UP 1)
(define DOWN -1)
(define CENTER 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lily specific variables.
(define-public default-script-alist '())

(define-public security-paranoia #f)
(if (not (defined? 'standalone))
    (define-public standalone (not (defined? 'ly-gulp-file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unassorted utility functions.

(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)))))

(define (other-axis a)
  (remainder (+ a 1) 2))
  

(define-public (widen-interval iv amount)
   (cons (- (car iv) amount)
         (+ (cdr iv) amount))
)



(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

(define (cons-map f x)
  "map F to contents of X"
  (cons (f (car x)) (f (cdr x))))

;; used where?
(define (reduce operator list)
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

(define (sign x)
  (if (= x 0)
      0
      (if (< x 0) -1 1)))

(define (write-me n x)
  (display n)
  (write x)
  (newline)
  x)

(define (!= l r)
  (not (= l r)))

(define (filter-list pred? list)
  "return that part of LIST for which PRED is true."
  (if (null? list) '()
      (let* ((rest  (filter-list pred? (cdr list))))
	(if (pred?  (car list))
	    (cons (car list)  rest)
	    rest))))

(define (filter-out-list pred? list)
  "return that part of LIST for which PRED is true."
  (if (null? list) '()
      (let* ((rest  (filter-list pred? (cdr list))))
	(if (not (pred?  (car list)))
	    (cons (car list)  rest)
	    rest))))

(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)))))

(define-public (uniq-list list)
  (if (null? list) '()
      (if (null? (cdr list))
	  list
	  (if (equal? (car list) (cadr list))
	      (uniq-list (cdr list))
	      (cons (car list) (uniq-list (cdr list)))))))

(define-public (alist<? x y)
  (string<? (symbol->string (car x))
	    (symbol->string (car y))))

(define-public (pad-string-to str wid)
  (string-append str (make-string (max (- wid (string-length str)) 0) #\ ))
  )

(define (ly-load x)
  (let* ((fn (%search-load-path x)))
    (if (ly-verbose)
	(format (current-error-port) "[~A]" fn))
    (primitive-load fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  output
(use-modules (scm tex)
	     (scm ps)
	     (scm pysk)
	     (scm ascii-script)
	     (scm sketch)
	     (scm pdftex)
	     )

(define output-alist
  `(
    ("tex" . ("TeX output. The default output form." ,tex-output-expression))
    ("ps" . ("Direct postscript. Requires setting GS_LIB and GS_FONTPATH" ,ps-output-expression))
    ("scm" . ("Scheme dump: debug scheme molecule expressions" ,write))
    ("as" . ("Asci-script. Postprocess with as2txt to get ascii art"  ,as-output-expression))
    ("sketch" . ("Bare bones Sketch output. Requires sketch 0.7" ,sketch-output-expression))
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

(if (not standalone)
    (map ly-load
					; load-from-path
	 '("output-lib.scm"
      	   "c++.scm"
	   "molecule.scm"
	   "bass-figure.scm"
	   "grob-property-description.scm"
	   "context-description.scm"
	   "interface-description.scm"
	   "beam.scm"
	   "clef.scm"
	   "slur.scm"
	   "font.scm"
	   "music-functions.scm"
	   "music-property-description.scm"
	   "auto-beam.scm"
	   "basic-properties.scm"
	   "chord-name.scm"
	   "grob-description.scm"
	   "translator-property-description.scm"
	   "script.scm"
	   "drums.scm"
	   "midi.scm"
	   )))

