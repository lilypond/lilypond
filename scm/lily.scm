;;; lily.scm -- implement Scheme output routines for TeX and PostScript
;;;
;;;  source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>


;;; This file contains various routines in Scheme that are easier to 
;;; do here than in C++.  At present it is an unorganised mess. Sorry. 


;;; We should repartition the entire scm side of lily in a
;;; more sane way, using namesspaces/modules?

(debug-enable 'backtrace)


(define point-and-click #f)

;;; library funtions

(use-modules (ice-9 regex))

(define (number-pair?  x)
  (and (pair? x) (number? (car x)) (number? (cdr x))))
(define (boolean-or-symbol? x) (or boolean? x) (or symbol? x))
(define (number-or-string? x) (or (number? x) (string? x)))
(define markup?
  (lambda (x) (or (string? x) (list? x))))

;; ugh: code dup ; merge.
(define (object-type obj)
  (cond
   ((dir? obj) "direction")
   ((number-pair? obj) "pair of numbers")
   ((ly-input-location? obj) "input location")   
   ((ly-grob? obj) "grob (GRaphical OBject)")
   ((pair? obj) "pair")
   ((integer? obj) "integer")
   ((list? obj) "list")
   ((symbol? obj) "symbol")
   ((string? obj) "string")
   ((boolean? obj) "boolean")
   ((moment? obj) "moment")
   ((number? obj) "number")
   ((char? obj) "char")
   ((input-port? obj) "input port")
   ((output-port? obj) "output port")   
   ((vector? obj) "vector")
   ((procedure? obj) "procedure") 
   ((boolean-or-symbol? obj) "boolean or symbol")
   ((number-or-string? obj) "number or string")
   ((markup? obj) "markup (list or string)")
   (else "unknown type")
  ))


(define (type-name  predicate)
  (cond
   ((eq? predicate dir?) "direction")
   ((eq? predicate number-pair?) "pair of numbers")
   ((eq? predicate ly-input-location?) "input location")   
   ((eq? predicate ly-grob?) "Grob")
   ((eq? predicate pair?) "pair")
   ((eq? predicate integer?) "integer")
   ((eq? predicate list?) "list")
   ((eq? predicate symbol?) "symbol")
   ((eq? predicate string?) "string")
   ((eq? predicate boolean?) "boolean")
   ((eq? predicate moment?) "moment")
   ((eq? predicate number?) "number")
   ((eq? predicate char?) "char")
   ((eq? predicate input-port?) "input port")
   ((eq? predicate output-port?) "output port")   
   ((eq? predicate vector?) "vector")
   ((eq? predicate procedure?) "procedure") 
   ((eq? predicate boolean-or-symbol?) "boolean or symbol")
   ((eq? predicate number-or-string?) "number or string")
   ((eq? predicate markup?) "markup (list or string)")
   (else "unknown type")
  ))


(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)
  ))))


;; The regex module may not be available, or may be broken.
(define use-regex
  (let ((os (string-downcase (vector-ref (uname) 0))))
    (not (equal? "cygwin" (substring os 0 (min 6 (string-length os)))))))

;; If you have trouble with regex, define #f
(define use-regex #t)
;;(define use-regex #f)

;; do nothing in .scm output
(define (comment s) "")

;; URG guile-1.3/1.4 compatibility
(define (ly-eval x) (eval2 x #f))

(define (comment s) "")

(define (mm-to-pt x)
  (* (/ 72.27 25.40) x)
  )

(define (cons-map f x)
  (cons (f (car x)) (f (cdr x))))

(define (reduce operator list)
      (if (null? (cdr list)) (car list)
	  (operator (car list) (reduce operator (cdr list)))
	  )
      )


(define (numbers->string l)
  (apply string-append (map ly-number->string l)))

; (define (chop-decimal x) (if (< (abs x) 0.001) 0.0 x))

(define (number->octal-string x)
  (let* ((n (inexact->exact x))
         (n64 (quotient n 64))
         (n8 (quotient (- n (* n64 64)) 8)))
    (string-append
     (number->string n64)
     (number->string n8)
     (number->string (remainder (- n (+ (* n64 64) (* n8 8))) 8)))))

(define (inexact->string x radix)
  (let ((n (inexact->exact x)))
    (number->string n radix)))


(define (control->string c)
  (string-append (number->string (car c)) " "
		 (number->string (cdr c)) " "))

(define (font i)
  (string-append
   "font"
   (make-string 1 (integer->char (+ (char->integer #\A) i)))
   ))

(define (scm-scm action-name)
  1)

(define security-paranoia #f)


;; silly, use alist? 
(define (find-notehead-symbol duration style)
  (case style
   ((cross) "2cross")
   ((harmonic) "0mensural")
   ((baroque) 
    (string-append (number->string duration)
		   (if (< duration 0) "mensural" "")))
   ((default) (number->string duration))
   (else
    (string-append (number->string duration) (symbol->string style))))
  )

(define (string-encode-integer i)
  (cond
   ((= i  0) "o")
   ((< i 0)   (string-append "n" (string-encode-integer (- i))))
   (else (string-append
	  (make-string 1 (integer->char (+ 65 (modulo i 26))))
	  (string-encode-integer (quotient i 26))
	  ))
   )
  )

(define default-script-alist '())

(define font-name-alist  '())
(define (tex-encoded-fontswitch name-mag)
  (let* (
	 (iname-mag (car name-mag))
	 (ename-mag (cdr name-mag))
	 )
    (cons iname-mag
	  (cons ename-mag
		(string-append  "magfont"
			  (string-encode-integer
			   (hashq (car ename-mag) 1000000))
			  "m"
			  (string-encode-integer
			   (inexact->exact (* 1000 (cdr ename-mag))))

			  )
		)
    )))

(define (define-fonts internal-external-name-mag-pairs)
  (set! font-name-alist (map tex-encoded-fontswitch
			     internal-external-name-mag-pairs))
  (apply string-append
	 (map (lambda (x)
		(font-load-command (car x) (cdr x)))
	      (map cdr font-name-alist)  

  )))

(define (fontify name-mag-pair exp)
  (string-append (select-font name-mag-pair)
		 exp)
  )

;;;;;;;;;;;;;;;;;;;;


; Make a function that checks score element for being of a specific type. 
(define (make-type-checker symbol)
  (lambda (elt)
    ;;(display  symbol)
    ;;(eq? #t (ly-get-elt-property elt symbol))
    (not (eq? #f (memq symbol (ly-get-elt-property elt 'interfaces))))
    ))


(define (arg->string arg)
  (cond ((number? arg) (inexact->string arg 10))
	((string? arg) (string-append "\"" arg "\""))
	((symbol? arg) (string-append "\"" (symbol->string arg) "\""))))

; ugh: naming.
(define (func name . args)
  (string-append 
   "(" name 
   (if (null? args) 
       ""
       (apply string-append 
	      (map (lambda (x) (string-append " " (arg->string x))) args)))
   ")\n"))

(define (sign x)
  (if (= x 0)
      1
      (if (< x 0) -1 1)))

(define (gulp-file name)
  (let* ((file (open-input-file name))
	 (text (read-delimited "" file)))
    (close file)
    text))

;; urg: Use when standalone, do:
;; (define ly-gulp-file scm-gulp-file)
(define (scm-gulp-file name)
  (set! %load-path 
	(cons (string-append (getenv 'LILYPONDPREFIX) "/ly")
	      (cons (string-append (getenv 'LILYPONDPREFIX) "/ps")
		    %load-path)))
  (let ((path (%search-load-path name)))
       (if path
	   (gulp-file path)
	   (gulp-file name))))

	
(define (index-cell cell dir)
  (if (equal? dir 1)
      (cdr cell)
      (car cell)))

(define major-scale
  '(
    (0 . 0)
    (1 . 0)
    (2 . 0)
    (3 . 0)
    (4 . 0)
    (5 . 0)
    (6 . 0)
    )
  )



;;
;; (name . (glyph clef-position octavation))
;; -- the name clefOctavation is misleading the value 7 is 1 octave not 7 Octaves.
;;
(define supported-clefs '(
	  ("treble" . ("clefs-G" -2 0))
	  ("violin" . ("clefs-G" -2 0))
	  ("G" . ("clefs-G" -2 0))
	  ("G2" . ("clefs-G" -2 0))
	  ("french" . ("clefs-G" -4  0))
	  ("soprano" . ("clefs-C" -4  0))
	  ("mezzosoprano" . ("clefs-C" -2  0))
	  ("alto" . ("clefs-C" 0 0))
	  ("tenor" . ("clefs-C" 2 0))
	  ("baritone" . ("clefs-C" 4  0))
	  ("varbaritone"  . ("clefs-F" 0 0))
	  ("bass" . ("clefs-F" 2  0))
	  ("F" . ( "clefs-F" 2 0))
	  ("subbass" . ("clefs-F" 4 0))

	  ;; should move mensural stuff to separate file? 
	  ("vaticana_do1" . ("clefs-vaticana_do" -1 0))
	  ("vaticana_do2" . ("clefs-vaticana_do" 1 0))
	  ("vaticana_do3" . ("clefs-vaticana_do" 3 0))
	  ("vaticana_fa1" . ("clefs-vaticana_fa" -1 0))
	  ("vaticana_fa2" . ("clefs-vaticana_fa" 1 0))
	  ("medicaea_do1" . ("clefs-medicaea_do" -1 0))
	  ("medicaea_do2" . ("clefs-medicaea_do" 1 0))
	  ("medicaea_do3" . ("clefs-medicaea_do" 3 0))
	  ("medicaea_fa1" . ("clefs-medicaea_fa" -1 0))
	  ("medicaea_fa2" . ("clefs-medicaea_fa" 1 0))
	  ("hufnagel_do1" . ("clefs-hufnagel_do" -1 0))
	  ("hufnagel_do2" . ("clefs-hufnagel_do" 1 0))
	  ("hufnagel_do3" . ("clefs-hufnagel_do" 3 0))
	  ("hufnagel_fa1" . ("clefs-hufnagel_fa" -1 0))
	  ("hufnagel_fa2" . ("clefs-hufnagel_fa" 1 0))
	  ("hufnagel" . ("clefs-hufnagel_do_fa" 4 0))
	  ("mensural1_c1" . ("clefs-mensural1_c" -4 0))
	  ("mensural1_c2" . ("clefs-mensural1_c" -2 0))
	  ("mensural1_c3" . ("clefs-mensural1_c" 0 0))
	  ("mensural1_c4" . ("clefs-mensural1_c" 2 0))
	  ("mensural2_c1" . ("clefs-mensural2_c" -4 0))
	  ("mensural2_c2" . ("clefs-mensural2_c" -2 0))
	  ("mensural2_c3" . ("clefs-mensural2_c" 0 0))
	  ("mensural2_c4" . ("clefs-mensural2_c" 2 0))
	  ("mensural2_c5" . ("clefs-mensural2_c" 4 0))
	  ("mensural3_c1" . ("clefs-mensural3_c" -2 0))
	  ("mensural3_c2" . ("clefs-mensural3_c" 0 0))
	  ("mensural3_c3" . ("clefs-mensural3_c" 2 0))
	  ("mensural3_c4" . ("clefs-mensural3_c" 4 0))
	  ("mensural_f" . ("clefs-mensural_f" 2 0))
	)
)

(define (clef-name-to-properties cl)
  (let ((e '())
	(oct 0)
	(l (string-length cl))
	)

    ;; ugh. cleanme
    (if (equal? "8" (substring cl (- l 1) l))
	(begin
	(if (equal? "^" (substring cl (- l 2) (- l 1)))
	    (set! oct 7)
	    (set! oct -7))
	
	(set! cl (substring cl 0 (- l 2)))))


    (set! e  (assoc cl supported-clefs))
    (if (pair? e)
	`(((symbol . clefGlyph)
	   (iterator-ctor . ,Property_iterator::constructor)
	   (value . ,(cadr e))
	   )
	  ((symbol . clefPosition)
	   (iterator-ctor . ,Property_iterator::constructor)
	   (value . ,(caddr e))
	   )
	  ,(if (not (equal? oct 0))
	       `((symbol . clefOctavation)
		 (iterator-ctor . ,Property_iterator::constructor)
		 (value . ,oct)
	       ))
	  )
	(begin
	  (ly-warn (string-append "Unknown clef type `" cl "'\nSee scm/lily.scm for supported clefs"))
	  '())
    )))



(define (repeat-name-to-ctor name)
  (let*
      ((supported-reps
	`(("volta" . ((iterator-ctor . ,Volta_repeat_iterator::constructor)
		      (length . ,Repeated_music::volta_music_length)
		      ))
	  ("unfold" . ((iterator-ctor . ,Unfolded_repeat_iterator::constructor)
		       (length . ,Repeated_music::unfolded_music_length)
		       ))
	  ("fold" . ((iterator-ctor  . ,Folded_repeat_iterator::constructor)
		      (length . ,Repeated_music::folded_music_length)
		      ))
	  ("tremolo" . ((iterator-ctor . ,Chord_tremolo_iterator::constructor)
			(length . ,Repeated_music::unfolded_music_length)
			))
	  ))
	  
       (handle (assoc name supported-reps))
       )

    (if (pair? handle)
	(cdr handle)
	(begin
	  (ly-warn
	   (string-append "Unknown repeat type `" name "'\nSee scm/lily.scm for supported repeats")
	   )
	  '(type . 'repeated-music))
	)
  ))


(map (lambda (x)   (eval-string (ly-gulp-file x)))
     '("tex.scm"
       "ps.scm"
;       "ascii-script.scm"
       "backend-property.scm"
       "translator-properties.scm"
       "interface.scm"
       "beam.scm"
       "slur.scm"
       "font.scm"
       "auto-beam.scm"
       "generic-property.scm"
       "basic-properties.scm"
       "chord-name.scm"
       "element-descriptions.scm"
       ))
