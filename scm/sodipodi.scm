;;;; sodipodi.scm -- implement Scheme output routines for PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; NOTE that Sodipodi
;;;;
;;;;  * dumps core on displaying feta characters
;;;;  * needs PFBs (ie, not PFAs like sketch)
;;;;  * must have (LilyPond/feta) fonts registered through GNOME's
;;;;    gnome-font-install (ie, not through X11, like sketch and xfontsel),
;;;;    which in turn is very picky about afm files
;;;;  * has it's own svg-like language: possibly this file should be
;;;;    moved to svg.scm


(debug-enable 'backtrace)


(define-module (scm sodipodi))
(define this-module (current-module))

(use-modules
 (guile)
 (lily))




;;; Lily output interface --- cleanup and docme

;;; Bare minimum interface for \score { \notes c } }
;;; should implement:
;;;
;;;    xx-output-expression
;;;    char
;;;    filledbox
;;;    placebox

;;; and should intercept: 
;;;
;;;    fontify
;;;    lily-def
;;;    header-end
;;;    define-fonts
;;;    no-origin
;;;    start-system
;;;    end-output
;;;    header
;;;    comment
;;;    stop-last-system



;; Module entry
;;(define-public (sodipodi-output-expression expr port)
;;  (display (eval expr this-module) port))

(define-public (sodipodi-output-expression expr port)
  (display (dispatch expr) port))


(define (dispatch expr)
  (let ((keyword (car expr)))
    (cond
     ((eq? keyword 'some-func) "")
     ;;((eq? keyword 'placebox) (dispatch (cadddr expr)))
     ;;((eq? keyword 'fontify) (dispatch (caddr expr)))
     (else
      (if (module-defined? this-module keyword)
	  (apply (eval keyword this-module) (cdr expr))
	  (begin
	    (display
	     (string-append "undefined: " (symbol->string keyword) "\n"))
	    ""))))))
  

;; Global vars

(define output-scale 1)

(define scale-to-unit
  (cond
   ((equal? (ly:unit) "mm") (/ 72.0  25.4))
   ((equal? (ly:unit) "pt") (/ 72.0  72.27))
   (else (error "unknown unit" (ly:unit)))))

;; alist containing fontname -> fontcommand assoc (both strings)
;;(define font-name-alist '())

;; Helper functions


(define (tagify tag string . attribute-alist)
  (string-append
   "<" tag
   (apply string-append (map (lambda (x) (string-append
					  " "
					  (symbol->string (car x))
					  "='"
					  (cdr x)
					  "'"))
			     attribute-alist))
   ">\n"
   string "\n</" tag ">\n"))


;; Interface functions

(define (char i)
  (if (or
       #t
       (= i #x9)
       (= i #xa)
       (= i #xd)
       (>= i #x20))
      ;;(tagify "tspan" (format #f "&#x~2,'0x;" i))
      (tagify "tspan" (format #f "&#xe0~2,'0x;" i))
      ;; how to access remaining characters??
      ;;;(tagify "tspan" (format #f "&#x~2,'0x;" #x20)
      (begin
	(format #t "can't display char: ~x\n" i)
	" ")))

(define (end-output)
  "</g></svg>")


(define (filledbox breapth width depth height)
  (tagify "rect" ""

	  '(style . "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-opacity:1;stroke-width:1pt;stroke-linejoin:miter;stroke-linecap:butt;")
	  `(x . ,(number->string (* output-scale (- 0 breapth))))
	  `(y . ,(number->string (* output-scale (- 0 height))))
	  `(width . ,(number->string (* output-scale (+ breapth width))))
	  `(height . ,(number->string (* output-scale (+ depth height))))))


(define font-alist '(("feta13" . ("LilyPond-Feta13" . "13"))
		     ("feta20" . "fill:black;stroke:none;font-family:lilypond;font-style:feta;font-weight:normal;font-size:20;fill-opacity:1;stroke-opacity:1;stroke-width:1pt;stroke-linejoin:miter;stroke-linecap:butt;text-anchor:start;writing-mode:lr;")
		     ("parmesan20" . "fill:black;stroke:none;font-family:lilypond;font-style:parmesan;font-weight:normal;font-size:20;fill-opacity:1;stroke-opacity:1;stroke-width:1pt;stroke-linejoin:miter;stroke-linecap:butt;text-anchor:start;writing-mode:lr;")
		     ))
(define (get-font name-mag-pair)
  ;; name-mag-pair: (quote ("feta20" . 0.569055118110236))"feta20"(quote ("feta20" . 0.569055118110236))
  (let ((f (assoc (caadr name-mag-pair) font-alist)))
    (if (pair? f)
	(cdr f)
	(begin
	  (format #t "font not found: ~s\n" (caadr name-mag-pair))
	  (cdr (assoc "feta20" font-alist))))))

(define (fontify name-mag-pair expr)
  (string-append
   (tagify "text" (dispatch expr) (cons 'style (get-font name-mag-pair)))))


(define (header creator generate)
"<?xml version='1.0' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 20010904//EN'
'http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd'
[
 <!ATTLIST svg
 xmlns:xlink CDATA #FIXED 'http://www.w3.org/1999/xlink'>
]>
<!-- Created with Sodipodi ('http://www.sodipodi.com/') -->
<svg
   id='svg1'
   sodipodi:version='0.26'
   xmlns='http://www.w3.org/2000/svg'
   xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd'
   xmlns:xlink='http://www.w3.org/1999/xlink'
   width='210mm'
   height='297mm'
   sodipodi:docbase='/tmp/'
   sodipodi:docname='/tmp/x'>
  <defs
     id='defs3' />
  <sodipodi:namedview
     id='base' />
  <g tranform='translate(50,-250)'>
  ")


(define (placebox x y expr)
  (tagify "g" (dispatch expr) `(transform .
					  ,(string-append
					    "translate(" (number->string
							  (* output-scale x))
					    ","
					    (number->string (- 0 (* output-scale y)))
					    ")"))))
				 
(define (lily-def key val)
  (if (equal? key "lilypondpaperoutputscale")
      ;; ugr
      (set! output-scale (* scale-to-unit (string->number val))))
  "")


