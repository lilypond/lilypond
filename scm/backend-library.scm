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
	 (silenced
	  (string-append command (if (ly:get-option 'verbose)
				     ""
				     " > /dev/null 2>&1 "))))

    (if (ly:get-option 'verbose)
	(format (current-error-port) (_ "Invoking `~a'...") command))
    
    (set! status (system silenced))
    (if (> status 0)
	(begin
	  (format (current-error-port) (_ "`~a' failed (~a)") command status)
	  (newline (current-error-port))
	  
	  ;; hmmm.  what's the best failure option? 
	  (throw 'ly-file-failed)))))

(define-public (sanitize-command-option str)
  (string-append
   "\""
   (regexp-substitute/global #f "[^- 0-9,.a-zA-Z'\"\\]" str 'pre 'post)
   "\""))

(define-public (postscript->pdf papersizename name)
  (let* ((cmd (format #f "ps2pdf -sPAPERSIZE=~a '~a'"
		      (sanitize-command-option papersizename) name))
	 (pdf-name (string-append (basename name ".ps") ".pdf" )))

    (if (access? pdf-name W_OK)
	(delete-file pdf-name))

    (format (current-error-port) (_ "Converting to `~a'...") pdf-name)
    (ly:system cmd)))

(define-public (postscript->png resolution papersizename name)
  (let* ((prefix (ly:effective-prefix))
	 ;; FIXME: should scripts/ps2png.py be installed in PREFIX?
	 (ps2png-source (if prefix
			   (format "~a/scripts/ps2png.py" prefix)
			   "ps2png"))
	 (cmd (format #f
		      "~a --resolution=~S --papersize=~a~a '~a'"
		      (if (file-exists? ps2png-source)
			  (format "python ~a" ps2png-source)
			  "ps2png")
		      resolution
		      (sanitize-command-option papersizename)
		      (if (ly:get-option 'verbose) " --verbose " "")
		      name)))
    ;; Do not try to guess the name of the png file,
    ;; GS produces PNG files like BASE-page%d.png.
    ;;(format (current-error-port) (_ "Converting to `~a'...")
    ;;	    (string-append (basename name ".ps") "-page1.png" )))
    (format (current-error-port) (_ "Converting to ~a...") "PNG")
    (newline (current-error-port))
    (ly:system cmd)))

(define-public (postprocess-output paper-book module filename formats)
  (for-each
   (lambda (f)
     ((eval (string->symbol (string-append "convert-to-" f))
	    module)
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

  (reverse new-fmts))

(define (header-to-file file-name key value)
  (set! key (symbol->string key))
  (if (not (equal? "-" file-name))
      (set! file-name (string-append file-name "." key)))
  (format (current-error-port)
	  (_ "Writing header field `~a' to `~a'...")
	  key
	  (if (equal? "-" file-name) "<stdout>" file-name))
  (if (equal? file-name "-")
      (display value)
      (display value (open-file file-name "w")))
  (newline (current-error-port))
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

