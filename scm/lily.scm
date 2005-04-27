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

(define-public (ly:load x)
  (let* ((fn (%search-load-path x)))
    (if (ly:get-option 'verbose)
	(format (current-error-port) "[~A]" fn))
    (primitive-load fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type-check-list location signature arguments)
  "Typecheck a list of arguments against a list of type
predicates. Print a message at LOCATION if any predicate failed."
  (define (recursion-helper signature arguments count) 
    (define (helper pred? arg count) 
      (if (not (pred? arg))

	  (begin
	    (ly:input-message location
			      (format #f
				      (_ "wrong type for argument ~a. Expecting ~a, found ~s")
				      count (type-name pred?) arg))
	    #f)
	  #t))

    (if (null? signature)
	#t
	(and (helper (car signature) (car arguments) count)
	     (recursion-helper (cdr signature) (cdr arguments) (1+ count)))
	))
  (recursion-helper signature arguments 1))
	 
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
     '("lily-library.scm"
       "define-music-types.scm"
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
  (let*
      ((status 0)

       (silenced
	(string-append command (if (ly:get-option 'verbose)
				 ""
				 " > /dev/null 2>&1 "))))
    
    (if (ly:get-option 'verbose)
	(format  (current-error-port) (_ "Invoking `~a'...\n") command))
    
    (set! status (system silenced))
    (if (> status 0)
	(begin
	  (format (current-error-port)
		  (_ "Error invoking `~a'. Return value ~a") silenced status)
	  (newline (current-error-port))))))

(define-public (sanitize-command-option str)
  (string-append
   "\""
   (regexp-substitute/global #f "[^- 0-9,.a-zA-Z'\"\\]" str 'pre 'post)
  "\""))

(define-public (postscript->pdf papersizename name)
  (let* ((cmd (string-append "ps2pdf "

			     (string-append
			      " -sPAPERSIZE="
			      (sanitize-command-option papersizename)
			      " "
			     name)))
	 (pdf-name (string-append (basename name ".ps") ".pdf" )))

    (if (access? pdf-name W_OK)
	(delete-file pdf-name))

    (format (current-error-port) (_ "Converting to `~a'...") pdf-name)
    (ly:system cmd)))

(define-public (postscript->png resolution name)
  (let
      ((cmd (string-append
	   "ps2png --resolution="
	   (if (number? resolution)
	       (number->string resolution)
	       "90 ")
	   (if (ly:get-option 'verbose)
	       " --verbose "
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


