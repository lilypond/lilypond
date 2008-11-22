#!@GUILE@ -s
!#
;;;; lilypond-invoke-editor.scm -- Invoke an editor in file:line:column mode
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2005--2007 Jan Nieuwenhuizen <janneke@gnu.org>

;; gui debug helper
;; (define (exit x) (system "sleep 10"))

(use-modules
 (ice-9 getopt-long)
 (ice-9 regex)
 (srfi srfi-1)
 (srfi srfi-13)
 (srfi srfi-14))

(define PROGRAM-NAME "lilypond-invoke-editor")
(define TOPLEVEL-VERSION "@TOPLEVEL_VERSION@")
(define DATADIR "@datadir@")
(define COMPILE-TIME-PREFIX
  (format #f "~a/lilypond/~a" DATADIR TOPLEVEL-VERSION))

;; argv0 relocation -- do in wrapper?

(define LILYPOND_DATADIR
  (let* ((prefix
	  (or (getenv "LILYPOND_DATADIR")
	      (dirname  (dirname (car (command-line)))))))
    

    (if (eq? prefix (dirname DATADIR)) COMPILE-TIME-PREFIX
	(format #f "~a/share/lilypond/~a"
		prefix TOPLEVEL-VERSION))))



;; gettext wrapper for guile < 1.7.2
(if (defined? 'gettext)
    (define-public _ gettext)
    (define-public (_ x) x))

(define (show-version port)
  (format port "~a (GNU LilyPond) ~a\n" PROGRAM-NAME TOPLEVEL-VERSION))

(define (show-help port)
  (format port (_ "Usage: lilypond-invoke-editor [textedit://]FILE:LINE:CHAR:COLUMN

Visit a file and position the cursor.

Options:
  -h, --help          show this help
  -v, --version       show version
")))

(define (parse-options args)
  (let* ((options (getopt-long args
			       '((help (single-char #\h))
				 (version (single-char #\v)))))
	 (files (cdr (assq '() options))))
    (if (assq 'help options)
	(begin
	  (show-version (current-output-port))
	  (show-help (current-output-port))
	(exit 0)))
    (if (assq 'version options)
	(begin (show-version (current-output-port)) (exit 0)))
    (show-version (current-error-port))
    files))

(define (re-sub re sub string)
  (regexp-substitute/global #f re string 'pre sub 'post))

;; FIXME: I'm going slowly but certainly mad; I really cannot find the
;; scm library function for this.
(define (unquote-uri uri)
  (re-sub "%([A-Fa-f0-9]{2})"
	  (lambda (m)
	    (string (integer->char (string->number (match:substring m 1) 16))))
	  uri))

(define (is-textedit-uri? uri)
  (string-match "^textedit://" uri))
  
  
(define (dissect-uri uri)
  (let* ((match (string-match "textedit://(.*):([^:]+):([^:]+):(.*)$" uri)))
    (if match
	(list (unquote-uri (match:substring match 1))
	      (match:substring match 2)
	      (match:substring match 3)
	      (match:substring match 4))
	(begin
	  (format (current-error-port) (_ "invalid textedit URI: ~a") uri)
	  (newline (current-error-port))
	  (format (current-error-port) (_ "expect: textedit://FILE:LINE:CHAR:COLUMN"))
	  (newline (current-error-port))
	  (exit 1)))))

(define PLATFORM
  (string->symbol
   (string-downcase
    (car (string-tokenize (vector-ref (uname) 0) char-set:letter)))))

(define (running-from-gui?)
  (let ((have-tty? (isatty? (current-input-port))))
    ;; If no TTY and not using safe, assume running from GUI.
    (not have-tty?)))

(define (run-editor uri)
  (let*
      ((command (apply get-editor-command (dissect-uri uri)))
       (status (system command)))
    (if (not (= status 0))
	(begin
	  (format (current-error-port)
		  (_ "failed to invoke editor: ~a") command)
	  (exit 1)))))

(define (run-browser uri)
  (system
   (if (getenv "BROWSER")
       (format "~a ~a" (getenv "BROWSER") uri)
       (format #f "firefox -remote 'OpenURL(~a,new-tab)'" uri))))


(define (strip-framework-path var)
  (define pat "lilypond/usr")
  (if (getenv var)
      (let*
	  ((val (getenv var))
	   (paths (string-split val #\:))
	   (without (remove (lambda (s) (string-contains s pat))
			    paths)))
	
	(if (not (= (length without)
		    (length paths)))
	    (setenv var (string-join without ":"))))))

(define (main args)
  (let ((files (parse-options args)))
    (if (running-from-gui?)
	(redirect-port (current-error-port)
		       (open-file (string-append
				   (or (getenv "TMP")
				       (getenv "TEMP")
				       "/tmp")
				   "/lilypond-invoke-editor.log") "a")))
    (if (not (= (length files) 1))
	(begin
	  (show-help (current-error-port))
	  (exit 2)))
    (set! %load-path (cons LILYPOND_DATADIR %load-path))

    (primitive-eval '(use-modules (scm editor)))

    (strip-framework-path "LD_LIBRARY_PATH")
    (let* ((uri (car files)))
      (if (is-textedit-uri? uri)
	  (run-editor uri)
	  (run-browser uri)))))

(main (command-line))
