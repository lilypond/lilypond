;;;; output-svg.scm -- implement Scheme output routines for SVG1
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2002--2004 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; http://www.w3.org/TR/SVG11/paths.html


;;; TODO: character selects by index from [custom] fonts

(debug-enable 'backtrace)
(define-module (scm output-svg))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (lily))

;; GLobals
;; FIXME: 2?
(define output-scale (* 2 scale-to-unit))
(define line-thickness 0)

(define (stderr string . rest)
  (apply format (cons (current-error-port) (cons string rest)))
  (force-output (current-error-port)))

(define (debugf string . rest)
  (if #f
      (apply stderr (cons string rest))))


(define (dispatch expr)
  (let ((keyword (car expr)))
    (cond
     ((eq? keyword 'some-func) "")
     ;;((eq? keyword 'placebox) (dispatch (cadddr expr)))
     (else
      (if (module-defined? this-module keyword)
	  (apply (eval keyword this-module) (cdr expr))
	  (begin
	    (display
	     (string-append "undefined: " (symbol->string keyword) "\n"))
	    ""))))))
  
;; Helper functions
(define (tagify tag string . attribute-alist)
  (string-append
   "<"
   tag
   (apply string-append
	  (map (lambda (x)
		 (string-append " " (symbol->string (car x)) "='" (cdr x) "'"))
	       attribute-alist))
   ">"
   string "</" tag ">\n"))

(define (control->list c)
  (list (car c) (cdr c)))

(define (control->string c)
  (string-append
   (number->string (car c)) ","
   ;; lose the -1
   (number->string (* -1 (cdr c))) " "))

(define (control-flip-y c)
  (cons (car c) (* -1 (cdr c))))

(define (ly:numbers->string l)
  (string-append
   (number->string (car l))
   (if (null? (cdr l))
       ""
       (string-append "," (ly:numbers->string (cdr l))))))

(define (svg-bezier l close)
  (let* ((c0 (car (list-tail l 3)))
	 (c123 (list-head l 3)))
    (string-append
     (if (not close) "M " "L ")
     (control->string c0)
     "C " (apply string-append (map control->string c123))
     (if (not close) "" (string-append
			 "L " (control->string close))))));; " Z")))))


(define (sqr x)
  (* x x))

(define (fontify font expr)
   (tagify "text" expr (cons 'style (svg-font font))))
;;	   (cons 'unicode-range "U+EE00-EEFF"))))

;;;;;;;;;;;;;;;;;;; share this utf8 stuff from output-gnome
;;;;;;;;;;;;;;;;;;;
(define (utf8 i)
  (cond
   ((< i #x80) (list (integer->char i)))
   ((< i #x800) (map integer->char
		     (list (+ #xc0 (quotient i #x40))
			   (+ #x80 (modulo i #x40)))))
   ((< i #x10000)
    (let ((x (quotient i #x1000))
	  (y (modulo i #x1000)))
      (map integer->char
	   (list (+ #xe0 x)
		 (+ #x80 (quotient y #x40))
		 (+ #x80 (modulo y #x40))))))
   (else FIXME)))
  
(define (custom-utf8 i)
  (if (< i 80)
      (utf8 i)
      (utf8 (+ #xee00 i))))

(define (string->utf8-string string)
  (list->string
   (apply append (map utf8 (map char->integer (string->list string))))))

(define (char->utf8-string char)
  (list->string (utf8 (char->integer char))))
;;  (list->string (custom-utf8 (char->integer char))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stencil outputters
;;;

;;; catch-all for missing stuff
;;; comment this out to see find out what functions you miss :-)
(define (dummy . foo) "")
(map (lambda (x) (module-define! this-module x dummy))
     (append
      (ly:all-stencil-expressions)
      (ly:all-output-backend-commands)))

(define (beam width slope thick blot)
  (let* ((x width)
	 (y (* slope width))
	 (z (sqrt (+ (sqr x) (sqr y)))))
    (tagify "rect" ""
	    `(style . ,(format "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-opacity:1;stroke-width:~f;stroke-linejoin:round;stroke-linecap:round;" line-thickness))
	    `(x . "0")
	    `(y . ,(number->string (* output-scale (- 0 (/ thick 2)))))
	    `(width . ,(number->string (* output-scale width)))
	    `(height . ,(number->string (* output-scale thick)))
	    ;;`(ry . ,(number->string (* output-scale half-lt)))
	    `(ry . ,(number->string (* output-scale (/ line-thickness 2))))
	    `(transform .
			,(format #f "matrix (~f, ~f, 0, 1, 0, 0) scale (~f, ~f)"
				 (/ x z)
				 (* -1 (/ y z))
				 1 1)))))

(define (bezier-sandwich l thick)
  (let* (;;(l (eval urg-l this-module))
	 (first (list-tail l 4))
	 (first-c0 (car (list-tail first 3)))
	 (second (list-head l 4)))
    (tagify "path" ""
	    `(stroke . "#000000")
	    `(stroke-width . ,(number->string line-thickness))
	    `(transform . ,(format #f "scale (~f, ~f)"
				   output-scale output-scale))
	    `(d . ,(string-append (svg-bezier first #f)
				  (svg-bezier second first-c0))))))

(define (char font i)
  (dispatch
   `(fontify ,font ,(tagify "tspan" (char->utf8-string
				     (integer->char i))))))

(define (comment s)
  (string-append "<!-- " s " !-->\n"))

(define (filledbox breapth width depth height)
  (round-filled-box breapth width depth height line-thickness))

(define (lily-def key val)
  (cond
   ((equal? key "lilypondpaperoutputscale")
    ;; ugr
    ;; If we just use transform scale (output-scale),
    ;; all fonts come out scaled too (ie, much too big)
    ;; So, we manually scale all other stuff.
    (set! output-scale (* scale-to-unit (string->number val))))
   ((equal? key "lilypondpaperlinethickness")
    (set! line-thickness (* scale-to-unit (string->number val)))))
  "")

(define (placebox x y expr)
  (tagify "g"
	  ;; FIXME -- JCN
	  ;;(dispatch expr)
	  expr
	  `(transform . ,(format #f "translate (~f, ~f)"
				 (* output-scale x)
				 (- 0 (* output-scale y))))))

(define (round-filled-box breapth width depth height blot-diameter)
  (tagify "rect" ""
	    `(style . ,(format "fill:#000000;fill-opacity:1;fill-rule:evenodd;stroke:#000000;stroke-opacity:1;stroke-width:~f;stroke-linejoin:miter;stroke-linecap:butt;" line-thickness))
	  `(x . ,(number->string (* output-scale (- 0 breapth))))
	  `(y . ,(number->string (* output-scale (- 0 height))))
	  `(width . ,(number->string (* output-scale (+ breapth width))))
	  `(height . ,(number->string (* output-scale (+ depth height))))
	  ;;`(ry . ,(number->string (* output-scale half-lt)))
	  `(ry . ,(number->string (/ blot-diameter 2)))))

(define (svg-font font)
   (define (font-family)
     (let ((name (ly:font-name font)))
       (if name
	   (regexp-substitute/global #f "^GNU-(.*)-[.0-9]*$" name 'pre 1 'post)
	   (begin
	     (stderr "font-name: ~S\n" (ly:font-name font))
	     ;; TODO s/filename/file-name/
	     (stderr "font-filename: ~S\n" (ly:font-filename font))
	     (stderr "font-size: ~S\n" (font-size))
	     "ecrm12"))))
   
   (define (font-size)
    (let* ((designsize (ly:font-design-size font))
	   (magnification (* (ly:font-magnification font)))
	   (scaling (* magnification designsize)))
      (debugf "scaling:~S\n" scaling)
      (debugf "magnification:~S\n" magnification)
      (debugf "design:~S\n" designsize)
      scaling))

   (format #f "font-family:~a;font-size:~a;fill:black;text-anchor:start;"
	   (font-family) (font-size)))

(define (text font string)
  (dispatch `(fontify ,font ,(tagify "tspan" (string->utf8-string string)))))

;; WTF is this in every backend?
(define (horizontal-line x1 x2 th)
  (filledbox (- x1) (- x2 x1) (* .5 th) (* .5 th)))
