;;;; backend-library.scm -- helpers for the backends.
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2005--2009 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@xs4all.nl>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; backend helpers.

(use-modules (scm ps-to-png)
	     (ice-9 optargs))

(define-public (ly:system command . rest)
  (let* ((status 0)
	 (dev-null "/dev/null")
	 (silenced (if (or (ly:get-option 'verbose)
			   (not (access? dev-null W_OK)))
		       command
		       (format #f "~a > ~a 2>&1 " command dev-null))))
    (if (ly:get-option 'verbose)
	(begin
	  (ly:message (_ "Invoking `~a'...") command))
	  (ly:progress "\n"))

    (set! status
	  (if (pair? rest)
	      (system-with-env silenced (car rest))
	      (system silenced)))
	
    (if (> status 0)
	(begin
	  (ly:message (_ "`~a' failed (~a)") command status)
	  (ly:progress "\n")
	  ;; hmmm.  what's the best failure option? 
	  (throw 'ly-file-failed)))))

(define-public (system-with-env cmd env)

  "Execute CMD in fork, with ENV (a list of strings) as the environment"
  (let*
      ;; laziness: should use execle?
      
      ((pid (primitive-fork)))
    (if (= 0 pid)
	;; child
	(begin
	  (environ env)
	  (system cmd))
	
	;; parent
	(cdr (waitpid pid)))))

(define-public (sanitize-command-option str)
  "Kill dubious shell quoting"
  
  (string-append
   "\""
   (regexp-substitute/global #f "[^-_ 0-9,.a-zA-Z'\"\\]" str 'pre 'post)
   "\""))

(define-public (search-executable names)
  (define (helper path lst)
    (if (null? (cdr lst))
	(car lst)
	(if (search-path path (car lst)) (car lst)
	    (helper path (cdr lst)))))

  (let ((path (parse-path (getenv "PATH"))))
    (helper path names)))

(define-public (search-gs)
  
  ;; must be sure that we don't catch stuff from old GUBs.
  (search-executable '("gs")))
  
(define-public (postscript->pdf paper-width paper-height name)
  (let* ((pdf-name (string-append
		    (dir-basename name ".ps" ".eps")
		    ".pdf"))
	 (is-eps (string-match "\\.eps$" name))
	 (paper-size-string (if is-eps
				"-dEPSCrop"
				(ly:format "-dDEVICEWIDTHPOINTS=~$\
 -dDEVICEHEIGHTPOINTS=~$"
					paper-width paper-height)))

	 (cmd (simple-format #f
		      "~a\
 ~a\
 ~a\
 ~a\
 -dCompatibilityLevel=1.4\
 -dNOPAUSE\
 -dBATCH\
 -r1200\
 -sDEVICE=pdfwrite\
 -sOutputFile=~S\
 -c .setpdfwrite\
 -f ~S\
"
		      (search-gs)
		      (if (ly:get-option 'verbose) "" "-q")
		      (if (or (ly:get-option 'gs-load-fonts)
			      (ly:get-option 'gs-load-lily-fonts))
			  "-dNOSAFER"
			  "-dSAFER")
		      paper-size-string
		      pdf-name
		      name)))
    ;; The wrapper on windows cannot handle `=' signs,
    ;; gs has a workaround with #.
    (if (eq? PLATFORM 'windows)
	(begin
	  (set! cmd (string-regexp-substitute "=" "#" cmd))
	  (set! cmd (string-regexp-substitute "-dSAFER " "" cmd))
	  (if (access? pdf-name W_OK)
	      (delete-file pdf-name))))

    (ly:message (_ "Converting to `~a'...") pdf-name)
    (ly:progress "\n")
    (ly:system cmd)))

(define-public (postscript->png resolution paper-width paper-height name)
  (let* ((verbose (ly:get-option 'verbose))
	 (rename-page-1 #f))

    ;; Do not try to guess the name of the png file,
    ;; GS produces PNG files like BASE-page%d.png.
    (ly:message (_ "Converting to ~a...") "PNG")
    (make-ps-images name
		    #:resolution resolution
		    #:page-width paper-width
		    #:page-height paper-height
		    #:rename-page-1 rename-page-1
		    #:be-verbose verbose
		    #:anti-alias-factor (ly:get-option 'anti-alias-factor)
		    #:pixmap-format (ly:get-option 'pixmap-format))
    (ly:progress "\n")))

(define-public (postprocess-output paper-book module filename formats)
  (let* ((completed (completize-formats formats))
	 (base (string-regexp-substitute "\\.[a-z]+$" "" filename))
	 (intermediate (remove (lambda (x) (member x formats)) completed)))
    
    (for-each (lambda (f)
		((eval (string->symbol (format "convert-to-~a" f))
		       module) paper-book filename)) completed)
    (if (ly:get-option 'delete-intermediate-files)
	(for-each (lambda (f)
		    (delete-file (string-append base "." f))) intermediate))))

(define-public (completize-formats formats)
  (define new-fmts '())
  (if (member "png" formats)
      (set! formats (cons "ps" formats)))
  (if (member "pdf" formats)
      (set! formats (cons "ps" formats)))
  (for-each (lambda (x)
	      (if (member x formats) (set! new-fmts (cons x new-fmts))))
	    '("ps" "pdf" "png"))
  (uniq-list (reverse new-fmts)))

(define (header-to-file file-name key value)
  (set! key (symbol->string key))
  (if (not (equal? "-" file-name))
      (set! file-name (string-append file-name "." key)))
  (ly:message (_ "Writing header field `~a' to `~a'...")
	      key
	      (if (equal? "-" file-name) "<stdout>" file-name))
  (if (equal? file-name "-")
      (display value)
      (let ((port (open-file file-name "w")))
	(display value port)
	(close-port port)))

  (ly:progress "\n")
  "")

(define-public (output-scopes scopes fields basename)
  (define (output-scope scope)
    (apply
     string-append
     (module-map
      (lambda (sym var)
	(let ((val (if (variable-bound? var) (variable-ref var) "")))
	  (if (and (memq sym fields) (string? val))
	      (header-to-file basename sym val))
	  ""))
      scope)))
  (apply string-append (map output-scope scopes)))

(define-public (backend-testing output-module)
  (define (missing-stencil-expression name)
    (begin
      (apply
       (if (ly:get-option 'warning-as-error) ly:error ly:warning)
       (list (_ "missing stencil expression `~S'") name))
      ""))

  (map (lambda (x)
	 (if (not (module-defined? output-module x))
	     (module-define! output-module x
			     (lambda* (#:optional y . z)
			       (missing-stencil-expression x)))))
       (ly:all-stencil-expressions)))
