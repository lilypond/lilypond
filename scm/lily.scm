;;;; lily.scm -- toplevel Scheme stuff
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2008 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@xs4all.nl>

;; Internationalisation: (_i "to be translated") gets an entry in the
;; POT file (gettext ) must be invoked explicitely to do the actual
;; "translation".
;;(define-macro (_i x) x)
;;(define-macro-public _i (x) x)
;;(define-public-macro _i (x) x)
;; Abbrv-PWR!
(defmacro-public _i (x) x)

(read-enable 'positions)
(debug-enable 'debug)

(define-public PLATFORM
  (string->symbol
   (string-downcase
    (car (string-tokenize (utsname:sysname (uname)))))))

(define scheme-options-definitions
  `(

    ;; NAMING: either

    ;; - [subject-]object-object-verb +"ing"
    ;; - [subject-]-verb-object-object

    (anti-alias-factor 1 "render at higher resolution and scale down result\nto prevent jaggies in PNG")
    (backend ps "which backend to use by default; Options: eps, null, ps [default], scm, svg)")
    (check-internal-types #f "check every property assignment for types")
    (clip-systems #f "Generate cut-out snippets of a score")
    (datadir #f "LilyPond prefix for data files (Readonly).")
    (debug-gc #f "dump memory debugging statistics")
    (debug-gc-assert-parsed-dead #f "for memory debugging:
ensure that all refs to parsed objects are dead.  This is an internal option, and is switched on automatically for -ddebug-gc.") 
    (debug-lexer #f "debug the flex lexer")
    (debug-page-breaking-scoring #f "dump scores for many different page breaking configurations")
    (debug-parser #f "debug the bison parser")
    (debug-property-callbacks #f "debug cyclic callback chains")
    (debug-skylines #f "debug skylines")
    (delete-intermediate-files #f
			       "delete unusable PostScript files")
    (dump-profile #f "dump memory and time information for each file")
    (dump-cpu-profile #f "dump timing information (system-dependent)")
    (dump-signatures #f "dump output signatures of each system.  Used for regression testing.")
    
    (eps-box-padding #f "Pad EPS bounding box left edge.  Guarantee alignment between systems in LaTeX.")
    (gs-load-fonts #f
		   "load fonts via Ghostscript.")
    (gs-load-lily-fonts #f
			"load only lilypond fonts via Ghostscript.")
    (gui #f "running from gui; redirect stderr to log file")
    (help #f "show this help.") 
    (include-book-title-preview #t "include book-titles in preview images.")
    (include-eps-fonts #t "Include fonts in separate-system EPS files.")
    (job-count #f "Process in parallel") 
    (log-file #f "redirect output to log FILE.log")
    (midi-extension ,(if (eq? PLATFORM 'windows)
			 "mid"
			 "midi")
		    "set the default file extension for MIDI")

    (old-relative #f
		  "relative for simultaneous music works
similar to chord syntax")
    (point-and-click #t "use point & click")
    (paper-size "a4" "the default paper size")
    (pixmap-format "png16m" "GS format to use for pixel images")
    (preview #f "make a incipit image. ")
    (print-pages #t "print pages normally. ")
    (protected-scheme-parsing #t "continue when finding errors in inline
scheme are caught in the parser. If off, halt 
on errors, and print a stack trace.")
    (profile-property-accesses #f "keep statistics of get_property() calls.")
    
    (resolution 101 "resolution for generating PNG bitmaps")
    (read-file-list #f "Read files to be processed from command line arguments")
    (relative-includes #f "When processing an \\include command, look for the included file
relative to the current file (instead of the root file)")

    (safe #f "Run safely")
    (strict-infinity-checking #f "If yes, crash on encountering Inf/NaN.")
    (strip-output-dir #t "If yes, strip directories from input files.")
    (separate-log-files #f "Output to FILE.log per file.")
    (trace-memory-frequency #f "Record Scheme cell usage this many times per second, and dump to file.")
    (trace-scheme-coverage #f "Record coverage of Scheme files") 
    (show-available-fonts #f
			  "List font names available.")
    (verbose ,(ly:command-line-verbose?) "value for the --verbose flag")
    ))

;; need to do this in the beginning. Other parts of the
;; Scheme init depend on these options.
;;
(for-each
 (lambda (x)
   (ly:add-option (car x) (cadr x) (caddr x)))
 scheme-options-definitions)

(for-each
 (lambda (x)
   (ly:set-option (car x) (cdr x)))
 (eval-string (ly:command-line-options)))

(debug-set! stack 0)

(if (defined? 'set-debug-cell-accesses!)
    (set-debug-cell-accesses! #f))

					;(set-debug-cell-accesses! 1000)

(use-modules (ice-9 regex)
	     (ice-9 safe)
	     (ice-9 format)
	     (ice-9 rdelim)
             (ice-9 optargs)
	     (oop goops)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (srfi srfi-14)
	     (scm clip-region)
	     (scm memory-trace)
	     (scm coverage)
	     )
(define-public fancy-format format)
(define-public (ergonomic-simple-format dest . rest)
  "Like ice-9 format, but without the memory consumption."
  
  (if (string? dest)
      (apply simple-format (cons #f (cons dest rest)))
      (apply simple-format (cons dest rest))))

(define format ergonomic-simple-format)

;; my display
(define-public (myd k v) (display k) (display ": ") (display v) (display ", ")
  v)

(define-public (print . args)
  (apply format (cons (current-output-port) args)))


;;; General settings
;;; debugging evaluator is slower.  This should
;;; have a more sensible default.

(if (or (ly:get-option 'verbose)
	(ly:get-option 'trace-memory-frequency)
	(ly:get-option 'trace-scheme-coverage)
	)
    (begin
      (ly:set-option 'protected-scheme-parsing #f)
      (debug-enable 'debug)
      (debug-enable 'backtrace)
      (read-enable 'positions)))


(if (ly:get-option 'trace-scheme-coverage)
    (coverage:enable))

(define-public parser #f)


;; gettext wrapper for guile < 1.7.2
(if (defined? 'gettext)
    (define-public _ gettext)
    (define-public _ ly:gettext))

(define-public (ly:load x)
  (let* ((file-name (%search-load-path x)))
    (if (ly:get-option 'verbose)
	(ly:progress "[~A" file-name))
    (if (not file-name)
	(ly:error (_ "cannot find: ~A") x))
    (primitive-load file-name)
    (if (ly:get-option 'verbose)
	(ly:progress "]"))))

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

(define-safe-public (lilypond-version)
  (string-join
   (map (lambda (x) (if (symbol? x)
			(symbol->string x)
			(number->string x)))
	(ly:version))
   "."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init pitch system

(ly:set-default-scale (ly:make-scale #(0 1 2 5/2 7/2 9/2 11/2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other files.


(define
  init-scheme-files
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
	    
	    "flag-styles.scm"
	    "fret-diagrams.scm"
	    "harp-pedals.scm"
	    "predefined-fretboards.scm"
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


(for-each ly:load init-scheme-files)


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
;; timing

(define (profile-measurements)
  (let* ((t (times))
	 (stats (gc-stats)))
    
    (list
     (- (+ (tms:cutime t)
	   (tms:utime t))
	(ly:assoc-get 'gc-time-taken stats))
     
     (ly:assoc-get 'total-cells-allocated  stats 0)
     )))

(define (dump-profile base last this)
  (let*
      ((outname (format "~a.profile" (dir-basename base ".ly")))
       (diff (map (lambda (y) (apply - y)) (zip this last))))
    
    (ly:progress "\nWriting timing to ~a..." outname)
    (format (open-file outname "w")
	    "time: ~a\ncells: ~a\n"
	    (if (ly:get-option 'dump-cpu-profile)
		(car diff)
		0)
	    (cadr diff)
	    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug mem leaks

(define gc-dumping #f)
(define gc-protect-stat-count 0)

(define-public (dump-live-object-stats outfile)
  (for-each
   (lambda (x)
     (format outfile "~a: ~a\n" (car x) (cdr x)))
   (sort (gc-live-object-stats)
	 (lambda (x y)
	   (string<? (car x) (car y))))))

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
	    (apply + (map (lambda (obj-count) (if (symbol? (car obj-count))
						  (cdr obj-count)
						  0))
			     protects)))	     

    ;; (display (ly:smob-protects))
    (newline outfile)
    (if (defined? 'gc-live-object-stats)
	(let* ((stats #f))
	  (display "Live object statistics: GC'ing\n")
	  (ly:reset-all-fonts)
	  (gc)
	  (gc)
	  (display "Asserting dead objects\n")
	  (ly:set-option 'debug-gc-assert-parsed-dead #t)
	  (gc)
	  (ly:set-option 'debug-gc-assert-parsed-dead #f)

	  (set! stats (gc-live-object-stats))
	  (display "Dumping live object statistics.\n")
	  (dump-live-object-stats outfile)))

    (newline outfile)
    (let*
	((stats (gc-stats)))
      
      (for-each
       (lambda (sym)
	 (display
	  (format "~a ~a ~a\n"
		  gc-protect-stat-count
		  sym
		  (let ((sym-stat (assoc sym stats)))
		    (if sym-stat 
			(cdr sym-stat)
			"?")))
	  outfile))
       '(protected-objects bytes-malloced cell-heap-size
			   
			   )))

    (set! gc-dumping #f)
    (close-port outfile)
    
    ))


(define (check-memory)
  "read /proc/self to check up on memory use." 
  (define (gulp-file name)
    (let* ((file (open-input-file name))
	   (text (read-delimited "" file)))
      (close file)
      text))
  (let*
      ((stat (gulp-file "/proc/self/status"))
       (lines (string-split stat #\newline))
       (interesting (filter identity
			    (map
			     (lambda (l)
			       (string-match "^VmData:[ \t]*([0-9]*) kB" l))
			     lines)))
       (mem (string->number (match:substring (car interesting) 1)))
       )

    
    (display (format  "VMDATA: ~a\n" mem))
    (display (gc-stats))
    (if (> mem 100000)
	(begin
	  (dump-gc-protects)
	  (raise 1)))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (multi-fork count)
  "Split this process in COUNT helpers. Returns either a list of pids,
or the number of the process."
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

  (eval-string (ly:command-line-code))

  (if (ly:get-option 'help)
      (begin
	(ly:option-usage)
	(exit 0)))

  (if (ly:get-option 'show-available-fonts)
      (begin
	(ly:font-config-display-fonts)
	(exit 0)
	))
  
  
  (if (ly:get-option 'gui)
      (gui-main files))

  (if (null? files)
      (begin
	(ly:usage)
	(exit 2)))

  (if (ly:get-option 'read-file-list)
      (set! files
	    (filter (lambda (s)
		      (> (string-length s) 0))
		    (apply append
			   (map (lambda (f) (string-split (ly:gulp-file f) #\nl))
				files)))
	    ))
  
  (if (and (number? (ly:get-option 'job-count))
	   (>= (length files) (ly:get-option 'job-count)))
      (let*
	  ((count (ly:get-option 'job-count))
	   (split-todo (split-list files count)) 
	   (joblist (multi-fork count))
	   (errors '()))

	(if (not (string-or-symbol? (ly:get-option 'log-file)))
	    (ly:set-option 'log-file "lilypond-multi-run"))
	
	(if (number? joblist)
	    (begin
	      (ly:set-option
	       'log-file (format "~a-~a"
				 (ly:get-option 'log-file) joblist))
	      (set! files (vector-ref split-todo joblist)))

	    (begin
	      (ly:progress "\nForking into jobs:  ~a\n" joblist)
	      (for-each
	       (lambda (pid)
		 (let* ((stat (cdr (waitpid pid))))
		   
		   (if (not (= stat 0))
		       (set! errors
			     (acons (list-element-index joblist pid)
				    stat errors)))))
	       joblist)

	      (for-each
	       (lambda (x)
		 (let* ((job (car x))
			(state (cdr x))
			(logfile (format "~a-~a.log"
					  (ly:get-option 'log-file) job))
			(log (ly:gulp-file logfile))
			(len (string-length log))
			(tail (substring  log (max 0 (- len 1024)))))

		   (if (status:term-sig state)
		       (ly:message
			"\n\n~a\n"
			(format (_ "job ~a terminated with signal: ~a")
				job (status:term-sig state)))
		       (ly:message
			(_ "logfile ~a (exit ~a):\n~a")
			logfile (status:exit-val state) tail))))

	       errors)

	      (if (pair? errors)
		  (ly:error "Children ~a exited with errors." (map car errors)))

	      ;; must overwrite individual entries
	      (if (ly:get-option 'dump-profile)
		  (dump-profile "lily-run-total" '(0 0) (profile-measurements)))

	    (exit (if (null? errors) 0 1))))))
	   
  (if (string-or-symbol? (ly:get-option 'log-file))
      (ly:stderr-redirect (format "~a.log" (ly:get-option 'log-file)) "w"))
  
  (let ((failed (lilypond-all files)))
    (if (ly:get-option 'trace-scheme-coverage)
	(begin
	  (coverage:show-all (lambda (f) (string-contains f "lilypond"))
			     )))
    
    (if (pair? failed)
	(begin
	  (ly:error (_ "failed files: ~S") (string-join failed))
	  (exit 1))
	(begin
	  ;; HACK: be sure to exit with single newline
	  (ly:message "")
	  (exit 0)))))

(define-public (lilypond-all files)
  (let* ((failed '())
	 (separate-logs (ly:get-option 'separate-log-files))
	 (ping-log
	  (if separate-logs
	      (open-file (if (string-or-symbol? (ly:get-option 'log-file))
			     (format "~a.log" (ly:get-option 'log-file))
			     "/dev/tty") "a") #f))
	 (do-measurements (ly:get-option 'dump-profile))
	 (handler (lambda (key failed-file)
		    (set! failed (append (list failed-file) failed)))))

    (gc)
    (for-each
     (lambda (x)
       (let*
	   ((start-measurements (if do-measurements
				    (profile-measurements)
				    #f))
	    (base (dir-basename x ".ly"))
	    (all-settings (ly:all-options)))

	 (if separate-logs
	     (ly:stderr-redirect (format "~a.log" base) "w"))
	 (if ping-log
	     (format ping-log "Procesing ~a\n" base))
	      
	 (if (ly:get-option 'trace-memory-frequency) 
	     (mtrace:start-trace  (ly:get-option 'trace-memory-frequency)))
	 
	 (lilypond-file handler x)
	 (if start-measurements
	     (dump-profile x start-measurements (profile-measurements)))

	 (if (ly:get-option 'trace-memory-frequency)
	     (begin
	       (mtrace:stop-trace)
	       (mtrace:dump-results base)))
	  	 
	 (for-each
	  (lambda (s)
	    (ly:set-option (car s) (cdr s)))
	  all-settings)

	 (ly:clear-anonymous-modules)
	 (ly:set-option 'debug-gc-assert-parsed-dead #t)
	 (gc)
	 (ly:set-option 'debug-gc-assert-parsed-dead #f)

	 
	 (if (ly:get-option 'debug-gc)
	     (dump-gc-protects)
	     (if (= (random 40) 1)
		 (ly:reset-all-fonts)))))

     files)

    ;; we want the failed-files notice in the aggregrate logfile.
    (if ping-log
	(format ping-log "Failed files: ~a\n" failed))
	 
    (if (ly:get-option 'dump-profile)
	(dump-profile "lily-run-total" '(0 0) (profile-measurements)))

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
      (let* ((base (dir-basename (car files) ".ly"))
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
