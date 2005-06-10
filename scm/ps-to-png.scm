;;;; ps-to-png.scm --
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2005 Jan Nieuwenhuizen <janneke@gnu.org>

(define-module (scm ps-to-png))

(use-modules
 (ice-9 optargs)
 (ice-9 regex)
 (ice-9 rw)
 (srfi srfi-1)
 (srfi srfi-13)
 (srfi srfi-14))

;; gettext wrapper for guile < 1.7.2
(if (defined? 'gettext)
    (define-public _ gettext)
    (define-public (_ x) x))

(define PLATFORM
  (string->symbol
   (string-downcase
    (car (string-tokenize (vector-ref (uname) 0) char-set:letter)))))

(define (re-sub re sub string)
  (regexp-substitute/global #f re string 'pre sub 'post))

(define (search-executable names)
  (define (helper path lst)
    (if (null? (cdr lst))
	(car lst)
	(if (search-path path (car lst)) (car lst)
	    (helper path (cdr lst)))))

  (let ((path (parse-path (getenv "PATH"))))
    (helper path names)))

(define (search-gs)
  (search-executable '("gs-nox" "gs-8.15" "gs")))

(define (gulp-port port max-length)
  (let ((str (make-string max-length)))
    (read-string!/partial str port)
    str))

(define (dir-listing dir-name)
  (define (dir-helper dir lst)
    (let ((e (readdir dir)))
      (if (eof-object? e) lst (dir-helper dir (cons e lst)))))
  (reverse (dir-helper (opendir dir-name) '())))

(define (dir-re dir re)
  (filter (lambda (x) (string-match re x)) (dir-listing dir)))

(define BOUNDING-BOX-RE
  "^%%BoundingBox: (-?[0-9]+) (-?[0-9]+) (-?[0-9]+) (-?[0-9]+)")

(define (get-bbox file-name)
  (let* ((bbox (string-append file-name ".bbox"))
	 ;; -sOutputFile does not work with bbox?
	 (cmd (format #t "gs\
 -sDEVICE=bbox\
 -q\
 -dNOPAUSE\
 ~S\
 -c showpage\
 -c quit 2>~S"
			  file-name bbox))
	 (status (system cmd))
	 (s (gulp-port (open-file bbox "r") 10240))
	 (m (string-match BOUNDING_BOX_RE s)))
    (display m)
    (newline)
    (if m
	(list->vector
	 (map (lambda (x) (string->number (car x))) (vector->list m)))
	#f)))

(define-public (make-ps-images ps-name . rest)
  (let-optional
   rest ((resolution 90)
	 (paper-size "a4")
	 (rename-page-1? #f)
	 (verbose? #f))
   (let* ((base (basename (re-sub "[.]e?ps" "" ps-name)))
	  (header (gulp-port (open-file ps-name "r") 10240))
	  (png1 (string-append base ".png"))
	  (pngn (string-append base "-page%d.png"))
	  (pngn-re (re-sub "%d" "[0-9]*" pngn))
	  (multi-page? (and (string-match "\n%%Pages: " header)
			    (not (string-match "\n%%Pages: 1\n" header))))
	  (output-file (if multi-page? pngn png1))

	  ;;png16m is because Lily produces color nowadays.
	  (cmd (if multi-page?
		   (format #f "~a\
 ~a\
 -dGraphicsAlphaBits=4\
 -dNOPAUSE\
 -dTextAlphaBits=4\
 -sDEVICE=png16m\
 -sOutputFile='~a'\
 -sPAPERSIZE=~a\
 -r~S\
 '~a'\
 -c showpage\
 -c quit" output-file paper-size resolution ps-name)
		   (format #f "~a\
 ~a\
 -s\
 -dGraphicsAlphaBits=4\
 -dEPSCrop\
 -dNOPAUSE\
 -dTextAlphaBits=4\
 -sDEVICE=png16m\
 -sOutputFile='~a'\
 -r~S\
 '~a'\
 -c quit"
			   (search-gs)
			   (if verbose? "" "-q")
			   output-file resolution ps-name)))
	  (foo (for-each delete-file (append (dir-re "." png1)
					     (dir-re "." pngn-re))))
	  (bar (if verbose?
		   (begin
		     (format (current-error-port) (_ "Invoking `~a'...") cmd)
		     (newline (current-error-port)))))
	  (baz 
	   ;; The wrapper on windows cannot handle `=' signs,
	   ;; gs has a workaround with #.
	   (if (eq? PLATFORM 'windows)
	       (begin
		 (set! cmd (re-sub "=" "#" cmd))
		 (set! cmd (re-sub "-dSAFER " "" cmd)))))
	  (status (system cmd)))
     (if (not (= status 0))
	 (begin
	   (map delete-file
		(append (dir-re "." png1) (dir-re "." pngn-re)))
	   (format (current-error-port)
		   (format #f (_ "~a exited with status: ~S") "GS" status))
	   (exit 1)))
     (if (and rename-page-1? multi-page?)
	 (rename (re-sub "%d" "1" pngn) png1))
     (append (dir-re "." png1) (dir-re "." pngn-re)))))
