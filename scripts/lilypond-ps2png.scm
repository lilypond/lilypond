#!@GUILE@ \
-e main -s
!#
;;;; lilypond-ps2png.scm -- Convert PostScript file to PNG images using GS
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2005--2006 Jan Nieuwenhuizen <janneke@gnu.org>

(use-modules
 (ice-9 getopt-long)
 (ice-9 regex)
 (srfi srfi-13))

(define PROGRAM-NAME "lilypond-ps2png")
(define TOPLEVEL-VERSION "@TOPLEVEL_VERSION@")
(define DATADIR "@datadir@")
(define COMPILE-TIME-PREFIX
  (format #f "~a/lilypond/~a" DATADIR TOPLEVEL-VERSION))

(define paper-size "a4")
(define resolution 90)
(define verbose? #f)
(define rename-page-1 #f)

;; argv0 relocation -- do in wrapper?
(define LILYPONDPREFIX
  (or (getenv "LILYPONDPREFIX")
      (let* ((bindir (dirname (car (command-line))))
	     (prefix (dirname bindir))
	     (lilypond-prefix
	      (if (eq? prefix (dirname DATADIR)) COMPILE-TIME-PREFIX
		  (format #f "~a/share/lilypond/~a"
			  prefix TOPLEVEL-VERSION))))
	lilypond-prefix)))

;; gettext wrapper for guile < 1.7.2
(if (defined? 'gettext)
    (define-public _ gettext)
    (define-public (_ x) x))

(define (show-version port)
  (format port "~a (GNU LilyPond) ~a \n" PROGRAM-NAME TOPLEVEL-VERSION))

(define (show-help port)
  (format port (_ "Usage: lilypond-ps2png FILE

Convert PostScript file to PNG images.

Options:
  -h, --help              show this help
  -P, --paper-size=PAPER  use paper size PAPER
  -R, --resolution=RES    use resolution RES
  -V, --verbose           be verbose
  -v, --version           show version
")))

(define (parse-options args)
  (let* ((options (getopt-long args
			       '((help (single-char #\h))
				 (verbose (single-char #\V))
				 (version (single-char #\v))
				 (paper-size (single-char #\P) (value #t))
				 ;; compatibility
				 (papersize (value #t))
				 (resolution (single-char #\R) (value #t)))))
	 (files (cdr (assq '() options))))
    (if (assq 'help options)
	(begin
	  (show-version (current-output-port))
	  (show-help (current-output-port))
	(exit 0)))
    (if (assq 'version options)
	(begin (show-version (current-output-port)) (exit 0)))
    (if (assq 'verbose options)
	(begin
	  (set! verbose? #t)
	  (debug-enable 'debug)
	  (debug-enable 'backtrace)))

    (let ((o (or (assq-ref 'paper-size options)
		 (assq-ref 'papersize options))))
	  (if o (set! paper-size o)))
    (let ((o (assq-ref 'resolution options)))
	  (if o (set! resolution o)))
    (show-version (current-error-port))
    files))

(define (main args)
  (let ((files (parse-options args)))
    (if (= (length files) 0)
	(begin
	  (show-help (current-error-port))
	  (exit 2)))
    (set! %load-path (cons LILYPONDPREFIX %load-path))
    (primitive-eval '(use-modules (scm ps-to-png)))
    (for-each
     (lambda
      (x)
      (let ((png-files
	     (make-ps-images x resolution paper-size rename-page-1 verbose?)))
	(format (current-error-port) (_ "Wrote `~a'") (string-join png-files))
	(newline (current-error-port))))
     files)))
