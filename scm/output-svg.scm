;;;; output-svg.scm -- implement Scheme output routines for SVG1
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c)  2002--2004 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; http://www.w3.org/TR/SVG11
;;;; http://www.w3.org/TR/SVG12/ -- page, pageSet in draft

;;;; TODO:
;;;;  * font selection: name, size, design size
;;;;  * .cff MUST NOT be in fc's fontpath?

;;;;  * inkscape page/pageSet support
;;;;  * inkscape SVG-font support
;;;;    - use fontconfig/fc-cache for now, see output-gnome.scm

(debug-enable 'backtrace)
(define-module (scm output-svg))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (lily)
 (srfi srfi-13))

;; GLobals
;; FIXME: 2?
(define output-scale (* 2 scale-to-unit))

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
  (format #f "<~S~a>\n" entity (attributes attributes-alist)))

(define-public (eoc entity . attributes-alist)
  (format #f "<~S~a/>\n" entity (attributes attributes-alist)))

(define-public (ec entity)
  (format #f "</~S>\n" entity))

(define-public (entity entity string . attributes-alist)
  (if (equal? string "")
      (apply eoc entity attributes-alist)
      (string-append
       (apply eo (cons entity attributes-alist)) string (ec entity))))

(define (offset->point o)
  (format #f " ~S,~S" (car o) (cdr o)))

(define (svg-bezier lst close)
  (let* ((c0 (car (list-tail lst 3)))
	 (c123 (list-head lst 3)))
    (string-append
     (if (not close) "M " "L ")
     (offset->point c0)
     "C " (apply string-append (map offset->point c123))
     (if (not close) "" (string-append
			 "L " (offset->point close))))))

(define (sqr x)
  (* x x))

(define (font-size font)
  (let* ((designsize (ly:font-design-size font))
	 (magnification (* (ly:font-magnification font)))
	 (scaling (* output-scale magnification designsize)))
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

;; FIXME: font can be pango font-name or smob
;;        determine size and style properly.
(define (svg-font font)
  (let ((family (if (string? font) font (font-family font)))
	(size (if (string? font) 12 (font-size font)))
	(anchor "west"))
    (format #f "font-family:~a;font-style:~a;font-size:~a;text-anchor:~a;"
	    family
	    (otf-style-mangling font family)
	    size anchor)))

(define (fontify font expr)
   (entity 'text expr (cons 'style (svg-font font))))

(define-public (otf-style-mangling font family)
  ;; Hmm, family is emmentaler20/26?
  (if (string=? (substring family 0 (min (string-length family) 10))
		"emmentaler")
      ;; urg; currently empty
      ;;(substring family 10)
      "20"
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

(define (rect-beam width slope thick blot-diameter)
  (let* ((x width)
	 (y (* slope width))
	 (z (/ y x)))
    (entity 'rect ""
	    ;; The stroke will stick out.  To use stroke,
	    ;; the stroke-width must be subtracted from all other dimensions.
	    ;;'(stroke-linejoin . "round")
	    ;;'(stroke-linecap . "round")
	    ;;`(stroke-width . ,blot-diameter)
	    ;;'(stroke . "red")
	    ;;'(fill . "orange")

	    `(x . 0)
	    `(y . ,(- (/ thick 2)))
	    `(width . ,width)
	    `(height . ,(+ thick (* (abs z) (/ thick 2))))
	    `(rx . ,(/ blot-diameter 2))
	    `(transform . ,(string-append
			    (format #f "matrix (1, ~f, 0, 1, 0, 0)" (- z))
			    (format #f " scale (~f, ~f)"
				    output-scale output-scale))))))

(define (beam width slope thick blot-diameter)
  (let* ((b blot-diameter)
	 (t (- thick b))
	 (w (- width b))
	 (h (* w slope)))
    (entity 'polygon ""
	    '(stroke-linejoin . "round")
	    '(stroke-linecap . "round")
	    `(stroke-width . ,blot-diameter)
	    '(stroke . "black")
	    '(fill . "black")
	    `(points . ,(string-join
			 (map offset->point
			      (list (cons (/ b 2) (/ t 2))
				    (cons (+ w (/ b 2)) (+ h (/ t 2)))
				    (cons (+ w (/ b 2)) (+ h (- (/ t 2))))
				    (cons (/ b 2) (- (/ t 2)))))))
	    `(transform
	      . ,(format #f "scale (~f, -~f)" output-scale output-scale)))))

(define (path-beam width slope thick blot-diameter)
  (let* ((b blot-diameter)
	 (t (- thick b))
	 (w (- width b))
	 (h (* w slope)))
    (entity 'path ""
	    '(stroke-linejoin . "round")
	    '(stroke-linecap . "round")
	    `(stroke-width . ,blot-diameter)
	    '(stroke . "black")
	    '(fill . "black")
	    `(d . ,(format #f "M ~S,~S l ~S,~S l ~S,~S l ~S,~S l ~S,~S"
			   (/ b 2) (/ t 2)
			   w (- h)
			   0 (- t)
			   (- w) h
			   0 t))
	  `(transform
	    . ,(format #f "scale (~f, ~f)" output-scale output-scale)))))

(define (bezier-sandwich lst thick)
  (let* ((first (list-tail lst 4))
	 (first-c0 (car (list-tail first 3)))
	 (second (list-head lst 4)))
    (entity 'path ""
	    '(stroke-linejoin . "round")
	    '(stroke-linecap . "round")
	    `(stroke-width . ,thick)
	    '(stroke . "black")
	    '(fill . "black")
	    `(d . ,(string-append (svg-bezier first #f)
				  (svg-bezier second first-c0)))
	  `(transform
	    . ,(format #f "scale (~f, -~f)" output-scale output-scale)))))

(define (char font i)
  (dispatch
   `(fontify ,font ,(entity 'tspan (char->entity (integer->char i))))))

(define-public (comment s)
  (string-append "<!-- " s " !-->\n"))

(define (dashed-line thick on off dx dy)
  (draw-line thick 0 0 dx dy))

(define (draw-line thick x1 y1 x2 y2)
  (entity 'line ""
	  '(stroke-linejoin . "round")
	  '(stroke-linecap . "round")
	  `(stroke-width . ,thick)
	  '(stroke . "black")
	  ;;'(fill . "black")
	  `(x1 . ,x1)
	  `(y1 . ,y1)
	  `(x2 . ,x2)
	  `(y2 . ,y2)
	  `(transform
	    . ,(format #f "scale (~f, -~f)" output-scale output-scale))))

;; WTF is this in every backend?
(define (horizontal-line x1 x2 th)
  (filledbox (- x1) (- x2 x1) (* .5 th) (* .5 th)))

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
				 (- (* output-scale y))))))

(define (polygon coords blot-diameter)
  (entity 'polygon ""
	  '(stroke-linejoin . "round")
	  '(stroke-linecap . "round")
	  `(stroke-width . ,blot-diameter)
	  '(stroke . "black")
	  ;;'(fill . "black")
	  `(points . ,(string-join
		       (map offset->point (ly:list->offsets '() coords))))
	  `(transform
	    . ,(format #f "scale (~f, -~f)" output-scale output-scale))))

(define (round-filled-box breapth width depth height blot-diameter)
  (entity 'rect ""
	  ;; The stroke will stick out.  To use stroke,
	  ;; the stroke-width must be subtracted from all other dimensions.
	  ;;'(stroke-linejoin . "round")
	  ;;'(stroke-linecap . "round")
	  ;;`(stroke-width . ,blot)
	  ;;'(stroke . "red")
	  ;;'(fill . "orange")

	  `(x . ,(- breapth))
	  `(y . ,(- height))
	  `(width . ,(+ breapth width))
	  `(height . ,(+ depth height))
	  `(ry . ,(/ blot-diameter 2))
	  `(transform
	    . ,(format #f "scale (~f, ~f)" output-scale output-scale))))

(define (text font string)
  (dispatch `(fontify ,font ,(entity 'tspan (string->entities string)))))

(define (utf8-string pango-font-description string)
  (dispatch `(fontify ,pango-font-description ,(entity 'tspan string))))
