#!@GUILE@ \
-e main -s
!#
;;;; tfm2afm.scm -- convert tfm to afm, with the aid of tfmtodit
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>

(debug-enable 'backtrace)

;;;; library funtions
(use-modules
  (ice-9 debug)
  (ice-9 format)
  (ice-9 getopt-long)
  (ice-9 string-fun)
  (ice-9 regex))

;;; Script stuff
(define program-name "tfm2afm")

(define cur-output-name "-")
(define cur-output-file '())

(define subst-version "@TOPLEVEL_VERSION@")

(define program-version 	
  (if (eq? subst-version (string-append "@" "TOPLEVEL_VERSION" "@"))
      "unknown"
      subst-version))

(define (show-version port)
  (display (string-append 
	    program-name " - LilyPond version " program-version "\n")
	   port))

(define (show-help)
  (display "Convert TFM to AFM

Usage: tfm2afm [OPTION]... TFM-FILE

Options:
  -h,--help          this help
  -o,--output=FILE   set output file
  -v,--version       show version

Example: tfm2afm `kpsewhich cmr10.tfm`
"))

(define (gulp-file name)
  (let* ((file (open-input-file name))
	 (text (read-delimited "" file)))
    (close file)
    text))
	 
(define (dump-file name text)
  (let ((file (open-output-file name)))
    (display text file)
    (close file)))

;; urg, this kind of naming costs too much indenting
(define (split c s r)
  (separate-fields-discarding-char c s r))


;;; Script entry point
(define (main args)
  (let ((options (getopt-long args
			      `((output (single-char #\o)
                                          (value #t))
			      	(help (single-char #\h))
			      	(version (single-char #\v))))))
    (define (opt tag default)
      (let ((pair (assq tag options)))
        (if pair (cdr pair) default)))

    (if (assq 'help options)
	(begin (show-version (current-output-port)) (show-help) (exit 0)))

    (if (assq 'version options)
	(begin (show-version (current-output-port)) (exit 0)))

    (show-version (current-error-port))
    (let ((output-name (opt 'output "-"))
	  (files (let ((foo (opt '() '())))
		      (if (null? foo) 
			  (list "-")
			  foo))))
	 (do-file (car files) output-name))))

(define (string->dim scale string)
  (/ (string->number string) scale))

;; C 0 ; WX 7 ; N  rests-0 ;  B 0 -3125 7
(define (afm-char scale number name width height depth)
  (let ((w (string->dim scale width))
	(h (string->dim scale height))
	(d (string->dim scale depth)))
    ;; ARG: can't find doco for (format): ~s prints string in quotes
    ;;(format "C ~s ; WX ~d ; N ~s ; B 0 ~,3f ~,3f ~,3f ;\n"
    ;;   number (inexact->exact w) name d w h)
    (string-append "C " number " ; "
		   (format "WX ~d ; " (inexact->exact w))
		   "N " name " ; "
		   (format "B 0 ~,3f ~,3f ~,3f ;\n" d w h))))

;; # width[,height[,depth[,italic_correction[,left_italic_correction[,subscript_correction]]]]]
(define (dit-to-afm-char scale x)
  (if (> (string-length x) 0)
      (let* ((l (split #\ht x list))
	     (name (car l))
	     (dimensions (append (split #\, (cadr l) list) '("0" "0" "0"))))
	(let ((number (substring name (+ (string-index name #\- ) 1)))
	      (width (car dimensions))
	      (height (cadr dimensions))
	      (depth (caddr dimensions)))
	  (afm-char scale number name width height depth)))
	""))

;;
;; Hmm, this is a 10-liner in awk,
;; what am I doing wrong?
;;
(define (do-file tfm-name output-name)
  (let* ((font (basename tfm-name '.tfm))
	 (afm-name (string-append font '.afm))
	 (dit-name (string-append font '.dit))
	 (chart-name (string-append font '.chart))
	 (chart (let loop ((i 0) (s ""))
		  (if (= i 256)
		      s
		      (let ((n (number->string i)))
			(loop (+ i 1) (string-append s n " Character-" n "\n")))))))
    
    (dump-file chart-name chart)
    
    (if (= 0 (primitive-fork))
	(execlp 'tfmtodit tfm-name tfm-name chart-name dit-name)
	(waitpid 0))
    
    (let* ((dit (gulp-file dit-name))
	   (sections (split #\np (regexp-substitute/global
				  #f
				  "name \|\ninternalname \|\nspacewidth \|\nchecksum\|\ndesignsize \|\nkernpairs\n\|\ncharset\n"
				  dit 'pre "\f" 'post)
			    list))
	   (dit-vector (list->vector (cdr sections))))
      
      (dump-file
       afm-name
       (let ((name (vector-ref dit-vector 0))
	     (internalname (vector-ref dit-vector 1))
	     (spacewidth (vector-ref dit-vector 2))
	     (checksum (vector-ref dit-vector 3))
	     (designsize (vector-ref dit-vector 4))
	     (kernpairs (vector-ref dit-vector 5))
	     (charset (split #\nl (vector-ref dit-vector 6) list)))
	 (let ((scale (/ (string->number designsize) 100)))
	   (string-append
	    "FontName cmr
StartFontMetrics
StartCharMetrics "
	    (number->string (- (length charset) 2))
	    "\n"
	    (apply string-append
		   (map (lambda (x) (dit-to-afm-char scale x))
			charset))
	    "EndCharMetrics
EndFontMetrics"
	    )))))))
     
    
  