;;;; lily.scm -- toplevel Scheme stuff
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2007 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@xs4all.nl>


(define (define-scheme-options)
  (for-each (lambda (x)
	      (ly:add-option (car x) (cadr x) (caddr x)))
	  
	    `(

	      ;; NAMING: either

	      ;; - [subject-]object-object-verb +"ing"
	      ;; - [subject-]-verb-object-object

	      (anti-alias-factor 1 "render at higher resolution and scale down result\nto prevent jaggies in PNG")
	      (check-internal-types #f "check every property assignment for types")
	      (clip-systems #f "Generate cut-out snippets of a score")
	      (debug-gc #f "dump memory debugging statistics")
	      (debug-gc-assert-parsed-dead
	       #f "for memory debugging: ensure that all refs to parsed objects are dead.")
	      (debug-lexer #f "debug the flex lexer")
	      (debug-midi #f "generate human readable MIDI")
	      (debug-parser #f "debug the bison parser")
	      (delete-intermediate-files #f
					 "delete unusable PostScript files")
	      (dump-signatures #f "dump output signatures of each system")
	      (dump-tweaks #f "dump page layout and tweaks for each score having the tweak-key layout property set.")
	      
	      (eps-box-padding #f "Pad EPS bounding box left edge by this much to guarantee alignment between systems")

	      (gs-load-fonts #f
			    "load fonts via Ghostscript.")
	      (gui #f "running from gui; redirect stderr to log file")

	      (include-book-title-preview #t "include book-titles in preview images.")
	      (include-eps-fonts #t "Include fonts in separate-system EPS files.")
	      (job-count #f "Process in parallel") 
	      (log-file #f "redirect output to log FILE.log")

	      (old-relative #f
			    "relative for simultaneous music works
similar to chord syntax")
	      (object-keys #f
			   "experimental mechanism for remembering tweaks")
	      (point-and-click #t "use point & click")
	      (paper-size "a4" "the default paper size")
	      (pixmap-format "png16m" "GS format to use for pixel images")
	      (protected-scheme-parsing #t "continue when finding errors in inline
scheme are caught in the parser. If off, halt 
on errors, and print a stack trace.")
	      (profile-property-accesses #f "keep statistics of get_property() calls.")
	      
	      (resolution 101 "resolution for generating bitmaps")
	      (read-file-list #f "Read files to be processed from command line arguments")

	      (safe #f "Run safely")
	      (strict-infinity-checking #f "If yes, crash on encountering Inf/NaN")

	      (ttf-verbosity 0
			     "how much verbosity for TTF font embedding?")

	      (show-available-fonts #f
				    "List  font names available.")

	      (verbose ,(ly:command-line-verbose?) "value for the --verbose flag")
	      )))


;; need to do this in the beginning. Other parts of the
;; Scheme init depend on these options.
;;
(define-scheme-options)

(debug-set! stack 0)

(if (defined? 'set-debug-cell-accesses!)
    (set-debug-cell-accesses! #f))

					;(set-debug-cell-accesses! 1000)

(use-modules (ice-9 regex)
	     (ice-9 safe)
	     (ice-9 format)
             (ice-9 optargs)
	     (oop goops)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (srfi srfi-14)
	     (scm clip-region)
	     )


(define-public (ergonomic-simple-format dest . rest)
  "Like ice-9 format, but without the memory consumption."
  
  (if (string? dest)
      (apply simple-format (cons #f (cons dest rest)))
      (apply simple-format (cons dest rest))))

(define-public fancy-format format)
(define format ergonomic-simple-format)

;; my display
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

(define-public tex-backend?
  (member (ly:output-backend) '("texstr" "tex")))

(define-public parser #f)

(define-public (lilypond-version)
  (string-join
   (map (lambda (x) (if (symbol? x)
			(symbol->string x)
			(number->string x)))
	(ly:version))
   "."))


;; TeX C++ code actually hooks into TEX_STRING_HASHLIMIT 
(define-public TEX_STRING_HASHLIMIT 10000000)



;; gettext wrapper for guile < 1.7.2
(if (defined? 'gettext)
    (define-public _ gettext)
    (define-public _ ly:gettext))

(define-public (ly:load x)
  (let* ((file-name (%search-load-path x)))
    (if (ly:get-option 'verbose)
	(ly:progress "[~A" file-name))
    (if (not file-name)
	(ly:error (_ "Can't find ~A" x)))
    (primitive-load file-name)
    (if (ly:get-option 'verbose)
	(ly:progress "]"))))

;; Cygwin
;; #(CYGWIN_NT-5.1 Hostname 1.5.12(0.116/4/2) 2004-11-10 08:34 i686)
;;
;; Debian
;; #(Linux hostname 2.4.27-1-686 #1 Fri Sep 3 06:28:00 UTC 2004 i686)
;;
;; Mingw
;; #(Windows XP HOSTNAME build 2600 5.01 Service Pack 1 i686)
;;

;; ugh, code dup.
(define-public PLATFORM
  (string->symbol
   (string-downcase
    (car (string-tokenize (vector-ref (uname) 0) char-set:letter)))))

(define-public DOS
  (let ((platform (string-tokenize
		   (vector-ref (uname) 0) char-set:letter+digit)))
    (if (null? (cdr platform)) #f
	(member (string-downcase (cadr platform)) '("95" "98" "me")))))

(case PLATFORM
  ((windows)
   (define native-getcwd getcwd)
   (define (slashify x)
     (if (string-index x #\\)
	 x
	 (string-regexp-substitute
	  "//*" "/"
	  (string-regexp-substitute "\\\\" "/" x))))
   ;; FIXME: this prints a warning.
   (define-public (ly-getcwd)
     (slashify (native-getcwd))))
  (else (define-public ly-getcwd getcwd)))

(define-public (is-absolute? file-name)
  (let ((file-name-length (string-length file-name)))
    (if (= file-name-length 0)
	#f
	(or (eq? (string-ref file-name 0) #\/)
	    (and (eq? PLATFORM 'windows)
		 (> file-name-length 2)
		 (eq? (string-ref file-name 1) #\:)
		 (eq? (string-ref file-name 2) #\/))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type-check-list location signature arguments)
  "Typecheck a list of arguments against a list of type
predicates. Print a message at LOCATION if any predicate failed."

  (define (recursion-helper signature arguments count) 
    (define (helper pred? arg count) 
      (if (not (pred? arg))

	  (begin
	    (ly:input-message
	     location
	     (format
	      #f (_ "wrong type for argument ~a.  Expecting ~a, found ~s")
	      count (type-name pred?) arg))
	    #f)
	  #t))

    (if (null? signature)
	#t
	(and (helper (car signature) (car arguments) count)
	     (recursion-helper (cdr signature) (cdr arguments) (1+ count)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Safe definitions utility
(define safe-objects (list))

(define-macro (define-safe-public arglist . body)
  "Define a variable, export it, and mark it as safe, ie usable in LilyPond safe mode.
The syntax is the same as `define*-public'."
  (define (get-symbol arg)
    (if (pair? arg)
        (get-symbol (car arg))
        arg))
  (let ((safe-symbol (get-symbol arglist)))
    `(begin
       (define*-public ,arglist
         ,@body)
       (set! safe-objects (cons (cons ',safe-symbol ,safe-symbol)
                                safe-objects))
       ,safe-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init pitch system

(ly:set-default-scale (ly:make-scale #(0 2 4 5 7 9 11)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other files.

(for-each ly:load
	  ;; load-from-path
	  '("lily-library.scm"
	    "file-cache.scm"
	    "define-event-classes.scm"
	    "define-music-types.scm"
	    "output-lib.scm"
	    "c++.scm"
	    "chord-ignatzek-names.scm"
	    "chord-entry.scm"
	    "chord-generic-names.scm"
	    "stencil.scm"
	    "markup.scm"
	    "music-functions.scm"
	    "part-combiner.scm"
	    "autochange.scm"
	    "define-music-properties.scm"
	    "auto-beam.scm"
	    "chord-name.scm"

	    "parser-ly-from-scheme.scm"
	    "ly-syntax-constructors.scm"
	    
	    "define-context-properties.scm"
	    "translation-functions.scm"
	    "script.scm"
	    "midi.scm"
	    "layout-beam.scm"
	    "parser-clef.scm"
	    "layout-slur.scm"
	    "font.scm"
	    "encoding.scm"
	    
	    "fret-diagrams.scm"
	    "define-markup-commands.scm"
	    "define-grob-properties.scm"
	    "define-grobs.scm"
	    "define-grob-interfaces.scm"
	    "define-stencil-commands.scm"
	    "titling.scm"
	    
	    "paper.scm"
	    "backend-library.scm"
	    "x11-color.scm"

	    ;; must be after everything has been defined
	    "safe-lily.scm"))


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
	(,ly:simple-closure? . "simple closure")
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
	(,rhythmic-location? . "rhythmic location")
	(,scheme? . "any type")
	(,string? . "string")
	(,symbol? . "symbol")
	(,vector? . "vector")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug mem leaks

(define gc-dumping #f)
(define gc-protect-stat-count 0)
(define-public (dump-gc-protects)
  (set! gc-protect-stat-count (1+ gc-protect-stat-count))
  (let* ((protects (sort
		    (hash-table->alist (ly:protects))
		    (lambda (a b)
		      (< (object-address (car a))
			 (object-address (car b))))))

	 (out-file-name (string-append
			 "gcstat-" (number->string gc-protect-stat-count)
			 ".scm"))
	 (outfile    (open-file  out-file-name  "w")))

    (set! gc-dumping #t)
    (display (format "Dumping GC statistics ~a...\n" out-file-name))
    (display
     (map (lambda (y)
	    (let ((x (car y))
		  (c (cdr y)))
	      (display 
	       (format "~a (~a) = ~a\n" (object-address x) c x)
	       outfile)))
	  (filter
	   (lambda (x)
	     (not (symbol? (car x))))
	   protects))
     outfile)

    (format outfile "\nprotected symbols: ~a\n"
	    (length (filter symbol?  (map car protects))))
    
	     

    ;; (display (ly:smob-protects))
    (newline outfile)
    (if (defined? 'gc-live-object-stats)
	(let* ((stats #f))
	  (display "Live object statistics: GC'ing\n")
	  (ly:reset-all-fonts)
	  (gc)
	  (gc)
	  (ly:set-option 'debug-gc-assert-parsed-dead #t)
	  (gc)
	  (ly:set-option 'debug-gc-assert-parsed-dead #f)

	  (set! stats (gc-live-object-stats))
	  (display "Dumping live object statistics.\n")
	  
	  (for-each
	   (lambda (x)
	     (format outfile "~a: ~a\n" (car x) (cdr x)))
	   (sort (gc-live-object-stats)
		 (lambda (x y)
		   (string<? (car x) (car y)))))))


    (newline outfile)
    (let*
	((stats (gc-stats)))
      
      (for-each
       (lambda (sym)
	 (display
	  (format "~a ~a ~a\n"
		  gc-protect-stat-count
		  sym
		  (cdr (assoc sym stats)))
	  outfile))
       '(protected-objects bytes-malloced cell-heap-size
			   
			   )))

    (set! gc-dumping #f)
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (multi-fork count)
  (define (helper count acc)
    (if (> count 0)
      (let*
	  ((pid  (primitive-fork)))
	(if (= pid 0)
	    (1- count)
	    (helper (1- count) (cons pid acc))))
      acc))
  (helper count '()))


(define-public (lilypond-main files)
  "Entry point for LilyPond."
  
  (define (no-files-handler)
    (ly:usage)
    (exit 2))

  (if (ly:get-option 'gui)
      (gui-main files))

  (if (null? files)
      (no-files-handler))

  (if (ly:get-option 'read-file-list)
      (set! files
	    (filter (lambda (s)
		      (> (string-length s) 0))
		    (apply append
			   (map (lambda (f) (string-split (ly:gulp-file f) #\nl))
				files)))
	    ))
  
  (if (and (number? (ly:get-option 'job-count))
	   (> (length files) (ly:get-option 'job-count)))
      
      (let*
	  ((count (ly:get-option 'job-count))
	   (split-todo (split-list files count)) 
	   (joblist (multi-fork count))
	   (errors '()))

	(if (not (string-or-symbol? (ly:get-option 'log-file)))
	    (ly:set-option 'log-file "lilypond-multi-run"))
	
	(if (number? joblist)
	    (begin
	      (ly:set-option 'log-file (format "~a-~a"
					       (ly:get-option 'log-file) joblist))
	      (set! files (vector-ref split-todo joblist)))

	    (begin
	      (ly:progress "\nForking into jobs:  ~a\n" joblist)
	      (for-each
	       (lambda (pid)
		 (let* ((stat (cdr (waitpid pid))))
		   
		   (if (not (= stat 0))
		       (set! errors (acons (list-element-index joblist pid) stat errors)))))
	       joblist)

	      (for-each
	       (lambda (x)
		 (let* ((job (car x))
			(state (cdr x))
			(logfile  (format "~a-~a.log"
					  (ly:get-option 'log-file) job))
			(log (ly:gulp-file logfile))
			(len (string-length log))
			(tail (substring  log (max 0 (- len 1024)))))

		   (if (status:term-sig state)
		       (ly:message "\n\n~a\n"
				   (format (_ "job ~a terminated with signal: ~a")
					   job
					   (status:term-sig state)))
		       (ly:message (_ "logfile ~a (exit ~a):\n~a") logfile (status:exit-val state) tail))))

	       errors)

	      (if (pair? errors)
		  (ly:error "Children ~a exited with errors." (map car errors)))

	    (exit (if (null? errors) 0 1))))))
	      
	   
  (if (string-or-symbol? (ly:get-option 'log-file))
      (ly:stderr-redirect (format "~a.log" (ly:get-option 'log-file)) "w"))
  
  (let ((failed (lilypond-all files)))
    (if (pair? failed)
	(begin
	  (ly:error (_ "failed files: ~S") (string-join failed))
	  (exit 1))
	(begin
	  ;; HACK: be sure to exit with single newline
	  (ly:message "")
	  (exit 0)))))

(define-public (lilypond-all files)


  (if (ly:get-option 'show-available-fonts)
      (begin
	(ly:font-config-display-fonts)
	(exit 0)
	))
  
  (let* ((failed '())
	 (first #t)
	 (handler (lambda (key failed-file)
		    (set! failed (append (list failed-file) failed)))))

    (for-each
     (lambda (x)

       ;; We don't carry info across file boundaries
       (if first
	   (set! first #f)
	   (gc))
       
       (lilypond-file handler x)
       (ly:clear-anonymous-modules)
       (if (ly:get-option 'debug-gc)
	   (dump-gc-protects)
	   (if (= (random 40) 1)
	       (ly:reset-all-fonts))))


     files)
    failed))

(define (lilypond-file handler file-name)
  (catch 'ly-file-failed
	 (lambda () (ly:parse-file file-name))
	 (lambda (x . args) (handler x file-name))))

(use-modules (scm editor))

(define-public (gui-main files)
  (if (null? files)
      (gui-no-files-handler))

  (if (not (string? (ly:get-option 'log-file)))
      (let* ((base (basename (car files) ".ly"))
	     (log-name (string-append base ".log")))
	(if (not (ly:get-option 'gui))
	    (ly:message (_ "Redirecting output to ~a...") log-name))
	(ly:stderr-redirect log-name "w")
	(ly:message "# -*-compilation-*-"))
    
    (let ((failed (lilypond-all files)))
      (if (pair? failed)
	  (begin
	    ;; ugh
	    (ly:stderr-redirect "foo" "r")
	    (system (get-editor-command log-name 0 0 0))
	    (ly:error (_ "failed files: ~S") (string-join failed))
	    ;; not reached?
	    (exit 1))
	  (exit 0)))))

(define (gui-no-files-handler)
  (let* ((ly (string-append (ly:effective-prefix) "/ly/"))
	 ;; FIXME: soft-code, localize
	 (welcome-ly (string-append ly "Welcome_to_LilyPond.ly"))
	 (cmd (get-editor-command welcome-ly 0 0 0)))
    (ly:message (_ "Invoking `~a'...") cmd)
    (system cmd)
    (exit 1)))
