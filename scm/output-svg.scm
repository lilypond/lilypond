;;;; output-svg.scm -- implement Scheme output routines for SVG1
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c)  2002--2004 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; http://www.w3.org/TR/SVG11

;;;; TODO:
;;;;  * missing stencils: line, dashed-line ...
;;;;  * rounded corners on stencils: rect, bezier (inkscape bug?)
;;;;  * inkscape page/pageSet support

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

(define indent-level 0)
(define (indent s . add) s)

;;(define (indentation indent str)
;;   (regexp-substitute/global #f "\(\n\)[ \t]*" str 'pre 1 indent 'post))

(define (indent s . add)
  (let ((before indent-level)
	(after (apply + (cons indent-level add)))
	(after? (and (not (null? add)) (> (car add) 0))))
    (set! indent-level after)
    (if after?
      (string-append (make-string before #\ ) s)
      (string-append (make-string after #\ ) s))))

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
(define-public (attributes attributes-alist)
  (apply string-append
	 (map (lambda (x) (format #f " ~s=\"~a\"" (car x) (cdr x)))
	      attributes-alist)))

(define-public (eo entity . attributes-alist)
  (indent (format #f "<~S~a>\n" entity (attributes attributes-alist)) 2))

(define-public (eoc entity . attributes-alist)
  (indent (format #f "<~S~a/>\n" entity (attributes attributes-alist))))

(define-public (ec entity)
  (indent (format #f "</~S>\n" entity) -2))

(define-public (entity entity string . attributes-alist)
  (if (equal? string "")
      (apply eoc entity attributes-alist)
      (string-append
       (apply eo (cons entity attributes-alist)) string (ec entity))))

(define (control->list c)
  (list (car c) (cdr c)))

(define (control->string c)
  (string-append
   (number->string (car c)) ","
   ;; lose the -1
   (number->string (* -1 (cdr c))) " "))

(define (control-flip-y c)
  (cons (car c) (* -1 (cdr c))))

(define (ly:numbers->string lst)
  (string-append
   (number->string (car lst))
   (if (null? (cdr lst))
       ""
       (string-append "," (ly:numbers->string (cdr lst))))))

(define (svg-bezier lst close)
  (let* ((c0 (car (list-tail lst 3)))
	 (c123 (list-head lst 3)))
    (string-append
     (if (not close) "M " "L ")
     (control->string c0)
     "C " (apply string-append (map control->string c123))
     (if (not close) "" (string-append
			 "L " (control->string close))))))

(define (sqr x)
  (* x x))

(define (font-size font)
  (let* ((designsize (ly:font-design-size font))
	 (magnification (* (ly:font-magnification font)))
	 (ops 2)
	 (scaling (* ops magnification designsize)))
    (debugf "scaling:~S\n" scaling)
    (debugf "magnification:~S\n" magnification)
    (debugf "design:~S\n" designsize)
    scaling))

(define (integer->entity integer)
  (format #f "&#x~x;" integer))
		   
(define (char->entity char)
  (integer->entity (char->integer char)))
		   
(define (string->entities string)
  (apply string-append
	 (map (lambda (x) (char->entity x)) (string->list string))))

(define (svg-font font)
  (let* ((encoding (ly:font-encoding font))
	 (anchor (if (memq encoding '(fetaMusic fetaBraces)) 'start 'start))
	 (family (font-family font)))
   (format #f "font-family:~a;font-style:~a;font-size:~a;text-anchor:~S;"
	   (otf-name-mangling font family)
	   (otf-style-mangling font family)
	   (font-size font) anchor)))

(define (fontify font expr)
   (entity 'text expr (cons 'style (svg-font font))))

;; FIXME
(define-public (otf-name-mangling font family)
  ;; Hmm, family is bigcheese20/26?
  (if (string=? (substring family 0 (min (string-length family) 9))
		"bigcheese")
      "LilyPond"
      (if (string=? family "aybabtu")
	  "LilyPondBraces"
	  family)))

(define-public (otf-style-mangling font family)
  ;; Hmm, family is bigcheese20/26?
  (if (string=? (substring family 0 (min (string-length family) 9))
		"bigcheese")
      (substring family 9)
      "Regular"))

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
    (entity 'rect ""
	    `(style . ,(format "stroke-linejoin:round;stroke-linecap:round;stroke-width:~f;" blot))
	    `(x . "0")
	    `(y . ,(number->string (* output-scale (- 0 (/ thick 2)))))
	    `(width . ,(number->string (* output-scale width)))
	    `(height . ,(number->string (* output-scale thick)))
	    `(ry . ,(number->string (* output-scale (/ blot 2))))
	    `(transform .
			,(format #f "matrix (~f, ~f, 0, 1, 0, 0) scale (~f, ~f)"
				 (/ x z)
				 (* -1 (/ y z))
				 1 1)))))

(define (bezier-sandwich lst thick)
  (let* ((first (list-tail lst 4))
	 (first-c0 (car (list-tail first 3)))
	 (second (list-head lst 4)))
    (entity 'path ""
	    `(style . ,(format "stroke-linejoin:round;stroke-linecap:round;stroke-width:~f;" thick))
	    `(transform . ,(format #f "scale (~f, ~f)"
				   output-scale output-scale))
	    `(d . ,(string-append (svg-bezier first #f)
				  (svg-bezier second first-c0))))))

(define (char font i)
  (dispatch
   `(fontify ,font ,(entity 'tspan (char->entity (integer->char i))))))

(define-public (comment s)
  (string-append "<!-- " s " !-->\n"))

(define (filledbox breapth width depth height)
  (round-filled-box breapth width depth height 0))

(define (named-glyph font name)
  (dispatch
   `(fontify ,font ,(entity 'tspan
			    (integer->entity
			     (ly:font-glyph-name-to-charcode font name))))))

(define (placebox x y expr)
  (entity 'g
	  ;; FIXME -- JCN
	  ;;(dispatch expr)
	  expr
	  `(transform . ,(format #f "translate (~f, ~f)"
				 (* output-scale x)
				 (- 0 (* output-scale y))))))

(define (round-filled-box breapth width depth height blot-diameter)
  (entity 'rect ""
	  `(style . ,(format "stroke-linejoin:round;stroke-linecap:round;stroke-width:~f;" blot-diameter))
	  `(x . ,(number->string (* output-scale (- 0 breapth))))
	  `(y . ,(number->string (* output-scale (- 0 height))))
	  `(width . ,(number->string (* output-scale (+ breapth width))))
	  `(height . ,(number->string (* output-scale (+ depth height))))
	  `(ry . ,(number->string (/ blot-diameter 2)))))

(define (text font string)
  (dispatch `(fontify ,font ,(entity 'tspan (string->entities string)))))

;; WTF is this in every backend?
(define (horizontal-line x1 x2 th)
  (filledbox (- x1) (- x2 x1) (* .5 th) (* .5 th)))
