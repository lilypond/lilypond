;;;; sodipodi.scm -- implement Scheme output routines for PostScript
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; NOTE:
;;;;
;;;; * Get mftrace 1.0.12 or newer

;;;; * Get sodipodi-cvs from 2002-11-23 or newer
;;;;
;;;; * Put in your ~/.sodipodi/private-fonts:
;;;;     mf/out/parmesan20.pfa,LilyPond Parmesan,LilyPond,
;;;;     mf/out/feta-nummer10.pfa,LilyPond Nummer,LilyPond,
;;;;     mf/out/feta20.pfa,LilyPond Feta,LilyPond,



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
(define system-x 1)
(define system-y 0)
(define line-thickness 0.1)
(define half-lt (/ line-thickness 2))


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


(define (ascii->string i) (make-string 1 (integer->char i)))
(define (ascii->upm-string i)
  (let* ((i+1 (+ i 1))
	 (u1 #xee)
	 (u2 (+ #x80 (quotient i+1 #x40)))
	 (u3 (+ #x80 (modulo i+1 #x40))))
    (apply string-append
	   (map ascii->string
		(list u1 u2 u3)))))

(define (control->list c)
  (list (car c) (cdr c)))

(define (control->string c)
  (string-append
   (number->string (* output-scale (car c))) ","
   (number->string (* -1 (* output-scale (cdr c)))) " "))

(define (control-flip-y c)
  (cons (car c) (* -1 (cdr c))))

(define (numbers->string l)
  (string-append
   (number->string (car l))
   (if (null? (cdr l))
       ""
       (string-append ","  (numbers->string (cdr l))))))

(define (svg-bezier l)
  (let* ((c0 (car (list-tail l 3)))
	 (c123 (list-head l 3)))
    (string-append
     "M " (control->string c0)
     "C " (apply string-append (map control->string c123)))))
     
	 
(define xml-header
"<?xml version='1.0' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 20010904//EN'
'http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd'
[
 <!ATTLIST svg
 xmlns:xlink CDATA #FIXED 'http://www.w3.org/1999/xlink'>
]>
"
;;"
)

(define svg-header
"<svg
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



;; Interface functions

(define (sqr x)
  (* x x))

(define (beam width slope thick)
  (let* ((x width)
	 (y (* slope width))
	 (z (sqrt (+ (sqr x) (sqr y)))))
    (tagify "rect" ""

	  '(style . "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-opacity:1;stroke-width:1pt;stroke-linejoin:miter;stroke-linecap:butt;")
	  `(x . ,(number->string (* output-scale half-lt)))
	  `(y . ,(number->string (* output-scale (- half-lt (/ thick 2)))))
	  `(width . ,(number->string (* output-scale width)))
	  `(height . ,(number->string (* output-scale thick)))
;;	  `(ry . ,(number->string (* output-scale half-lt)))
	  `(ry . ,(number->string line-thickness))
	  `(transform . ,(format #f "matrix(~f,~f,0,1,0,0)"
				 (/ x z)
				 (* -1 (/ y z)))))))

;; TODO: bezier-ending, see ps.scm
(define (bezier-bow l thick)
  (bezier-sandwich l thick))

(define (bezier-sandwich l thick)
  (let* ((urg (eval l this-module))
	 (first (list-tail urg 4))
	 (second (list-head urg 4)))
    (string-append
     "<path\n"
     "style='stroke-width:"
     (number->string (* output-scale line-thickness)) ";'\n"
     "d='"
     (svg-bezier first)
     (svg-bezier second)
     "'/>\n")))
  
(define (char i)
  (if #t
      ;;(tagify "tspan" (format #f "&#xe0~2,'0x;" i))
      (tagify "tspan" (ascii->upm-string i))
      (begin
	(format #t "can't display char: ~x\n" i)
	" ")))


(define (comment s)
  (string-append "<!-- " s " -->\n"))

(define (define-fonts internal-external-name-mag-pairs)
  (comment (format #f "Fonts used: ~S" internal-external-name-mag-pairs)))

(define (end-output)
  "</g></svg>")

(define (filledbox breapth width depth height)
  (roundfilledbox breapth width depth height line-thickness))

(define font-cruft
  "fill:black;stroke:none;font-style:normal;font-weight:normal;text-anchor:start;writing-mode:lr;")

;; FIXME
(define font-alist
  `(  
    ("cmr8" . ,(string-append
		  font-cruft
		  "font-family:cmr;font-size:8;"))
    ("feta13" . ,(string-append
		  font-cruft
		  "font-family:LilyPond-Feta;font-size:13;"))
    ("feta-nummer10" . ,(string-append
			 font-cruft
			 "font-family:LilyPond-Feta-nummer;font-size:13;"))
    ("feta20" . ,(string-append
		  font-cruft
		  "font-family:LilyPond-Feta;font-size:20;"))
    ("parmesan20" . ,(string-append
		      font-cruft
		      "font-family:LilyPond-Parmesan;font-size:20;"))))

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

(define (header-end)
  (comment "header-end"))

(define (header creator generate)
  (string-append
   xml-header
   (comment creator)
   (comment generate)
   svg-header))
  

(define (lily-def key val)
  (if (equal? key "lilypondpaperoutputscale")
      ;; ugr
      (set! output-scale (* scale-to-unit (string->number val))))
  "")

(define (no-origin)
  "")


(define (placebox x y expr)
  (tagify "g" (dispatch expr)
	  `(transform .
		      ,(string-append
			"translate("
			;; urg
			;; (number->string (* output-scale x))
			(number->string (* output-scale (+ system-x x)))
			","
			;; urg
			;; (number->string (- 0 (* output-scale y)))
			(number->string (* output-scale (- system-y y)))
			")"))))

(define (roundfilledbox breapth width depth height blot-diameter)
  (tagify "rect" ""

	  '(style . "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:none;stroke-opacity:1;stroke-width:1pt;stroke-linejoin:miter;stroke-linecap:butt;")
	  `(x . ,(number->string (* output-scale (- 0 breapth))))
	  `(y . ,(number->string (* output-scale (- 0 height))))
	  `(width . ,(number->string (* output-scale (+ breapth width))))
	  `(height . ,(number->string (* output-scale (+ depth height))))
	  ;;`(ry . ,(number->string (* output-scale half-lt)))
	  `(ry . ,(number->string blot-diameter))))


  
;; TODO: use height, set scaling?
(define (start-system width height)
  (let ((y system-y))
    ;;"<g tranform='translate(50,-250)'>
  (set! system-y (+ system-y height))
  ;;(format #f "<g tranform='translate(0,~1,'~f)'>" y)))
  (string-append
   "\n"
   (comment "start-system")
   (comment "URG, transform does not work!")
   (format #f "<g tranform='translate(0.0,~f)'>\n" (* output-scale y)))))
  
(define (stop-system)
  (string-append
   "\n"
   (comment "stop-system")
   "</g>\n"))

(define stop-last-system stop-system)

(define (text s)
  ;; to unicode or not?
  (if #t
      (tagify "tspan" s)
      (tagify "tspan"
	      (apply string-appendb
		     (map (lambda (x) (ascii->upm-string (char->integer x)))
			  (string->list s))))))
