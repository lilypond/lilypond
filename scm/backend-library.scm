;;;; backend-library.scm -- helpers for the backends.
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2005 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@cs.uu.nl>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; backend helpers.

(define-public (ly:system command)
  (let* ((status 0)
	 (dev-null "/dev/null")
	 (silenced (if (or (ly:get-option 'verbose)
			   (not (access? dev-null W_OK)))
		       command
		       (format #f "~a > ~a 2>&1 " command dev-null))))
    (if (ly:get-option 'verbose)
	(ly:message (_ "Invoking `~a'...") command))
    
    (set! status (system silenced))
    (if (> status 0)
	(begin
	  (ly:message (_ "`~a' failed (~a)") command status)
	  (ly:progress "\n")
	  ;; hmmm.  what's the best failure option? 
	  (throw 'ly-file-failed)))))

(define-public (sanitize-command-option str)
  (string-append
   "\""
   (regexp-substitute/global #f "[^- 0-9,.a-zA-Z'\"\\]" str 'pre 'post)
   "\""))

(define-public (postscript->pdf papersizename name)
  (let* ((pdf-name (string-append (basename name ".ps") ".pdf" ))
	 (cmd (format #f
		      "gs\
 -dSAFER\
 -dCompatibilityLevel=1.4 \
 -sPAPERSIZE=~a\
 -q\
 -dNOPAUSE\
 -dBATCH\
 -r1200 \
 -sDEVICE=pdfwrite\
 -sOutputFile=~S\
 -c .setpdfwrite\
 -f ~S\
"
		      (sanitize-command-option papersizename)
		      pdf-name
		      name)))
    ;; The wrapper on windows cannot handle `=' signs,
    ;; gs has a workaround with #.
    (if (eq? PLATFORM 'windows)
	(begin
	  (set! cmd (string-regexp-substitute "=" "#" cmd))
	  (set! cmd (string-regexp-substitute "-dSAFER " "" cmd))))

    (if (access? pdf-name W_OK)
	(delete-file pdf-name))

    (ly:message (_ "Converting to `~a'...") pdf-name)
    (ly:progress "\n")
    (ly:system cmd)
    (if (running-from-gui?) (delete-file name))))

(define-public (postscript->png resolution papersizename name)
  (let* ((prefix (ly:effective-prefix))

	 ;; run the source, if  we are in the build-directory
	 (ps2png-source (if prefix
			   (format "~a/scripts/lilypond-ps2png.py" prefix)
			   "lilypond-ps2png"))
	 (cmd (format #f
		      "~a --resolution=~S --papersize=~a~a ~S"
		      (if (file-exists? ps2png-source)
			  (format "python ~a" ps2png-source)
			  "lilypond-ps2png")
		      resolution
		      (sanitize-command-option papersizename)
		      (if (ly:get-option 'verbose) " --verbose " "")
		      name)))
    ;; Do not try to guess the name of the png file,
    ;; GS produces PNG files like BASE-page%d.png.
    ;;(ly:message (_ "Converting to `~a'...")
    ;;	    (string-append (basename name ".ps") "-page1.png" )))
    (ly:message (_ "Converting to ~a...") "PNG")
    (ly:system cmd)
    (ly:progress "\n")))

(define-public (postprocess-output paper-book module filename formats)
  (for-each
   (lambda (f)
     ((eval (string->symbol (string-append "convert-to-" f)) module)
      paper-book filename))
   formats))

(define-public (completize-formats formats)
  (define new-fmts '())

  (if (member "png" formats)
      (set! formats (cons "ps" formats)))
  (if (member "pdf" formats)
      (set! formats (cons "ps" formats)))

  (for-each
   (lambda (x)
     (if (member x formats) (set! new-fmts (cons x new-fmts))))
   '("tex" "dvi" "ps" "pdf" "png"))

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
      (display value (open-file file-name "w")))
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

