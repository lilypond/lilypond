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
  (let ((cmd (string-append
	      "ps2png --resolution="
	      (if (number? resolution)
		  (number->string resolution)
		  "90 ")
	      (if (ly:get-option 'verbose)
		  "--verbose "
		  " ")
	      name)))

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

  new-fmts)

