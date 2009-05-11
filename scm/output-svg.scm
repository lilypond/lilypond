;;;; output-svg.scm -- implement Scheme output routines for SVG1
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2002--2009 Jan Nieuwenhuizen <janneke@gnu.org>

;;;; http://www.w3.org/TR/SVG11
;;;; http://www.w3.org/TR/SVG12/ -- page, pageSet in draft

;;;; TODO:
;;;;  * .cff MUST NOT be in fc's fontpath.
;;;;    - workaround: remove mf/out from ~/.fonts.conf,
;;;;      instead add ~/.fonts and symlink all /mf/out/*otf there.
;;;;    - bug in fontconfig/freetype/pango?

;;;;  * inkscape page/pageSet support
;;;;  * inkscape SVG-font support
;;;;    - use fontconfig/fc-cache for now, see output-gnome.scm

(define-module (scm output-svg))
(define this-module (current-module))

(use-modules
 (guile)
 (ice-9 regex)
 (ice-9 format)
 (lily)
 (srfi srfi-1)
 (srfi srfi-13))

(define fancy-format format)
(define format ergonomic-simple-format)

(define lily-unit-length 1.75)

(define (dispatch expr)
  (let ((keyword (car expr)))
    (cond
     ((eq? keyword 'some-func) "")
     ;;((eq? keyword 'placebox) (dispatch (cadddr expr)))
     (else
      (if (module-defined? this-module keyword)
	  (apply (eval keyword this-module) (cdr expr))
	  (begin
	    (ly:warning (_ "undefined: ~S") keyword)
	    ""))))))

;; Helper functions
(define-public (attributes attributes-alist)
  (apply string-append
	 (map (lambda (x) (format " ~s=\"~a\"" (car x) (cdr x)))
	      attributes-alist)))

(define-public (eo entity . attributes-alist)
  "o = open"
  (format "<~S~a>\n" entity (attributes attributes-alist)))

(define-public (eoc entity . attributes-alist)
  " oc = open/close"
  (format "<~S~a/>\n" entity (attributes attributes-alist)))

(define-public (ec entity)
  "c = close"
  (format "</~S>\n" entity))



(define-public (entity entity string . attributes-alist)
  (if (equal? string "")
      (apply eoc entity attributes-alist)
      (string-append
       (apply eo (cons entity attributes-alist)) string (ec entity))))

(define (offset->point o)
  (format " ~S,~S" (car o)  (- (cdr o))))

(define (number-list->point lst)
  (define (helper lst)
    (if (null? lst)
	'()
	(cons (format "~S,~S" (car lst) (cadr lst))
	      (helper (cddr lst)))))

  (string-join (helper lst) " "))  


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

(define (integer->entity integer)
  (fancy-format "&#x~x;" integer))

(define (char->entity char)
  (integer->entity (char->integer char)))

(define (string->entities string)
  (apply string-append
	 (map (lambda (x) (char->entity x)) (string->list string))))

(define svg-element-regexp
  (make-regexp "^(<[a-z]+) (.*>)"))

(define pango-description-regexp-comma
  (make-regexp ",( Bold)?( Italic)?( Small-Caps)? ([0-9.]+)$"))

(define pango-description-regexp-nocomma
  (make-regexp "( Bold)?( Italic)?( Small-Caps)? ([0-9.]+)$"))

(define (pango-description-to-svg-font str expr)
  (define alist '())
  (define (set-attribute attr val)
    (set! alist (assoc-set! alist attr val)))
  (let*
    ((match-1 (regexp-exec pango-description-regexp-comma str))
     (match-2 (regexp-exec pango-description-regexp-nocomma str))
     (match (if match-1
		match-1
		match-2)))

    (if (regexp-match? match)
	(begin
	  (set-attribute 'font-family (match:prefix match))
	  (if (string? (match:substring match 1))
	      (set-attribute 'font-weight "bold"))
	  (if (string? (match:substring match 2))
	      (set-attribute 'font-style "italic"))
	  (if (string? (match:substring match 3))
	      (set-attribute 'font-variant "small-caps"))
	  (set-attribute 'font-size
			 (/ (string->number (match:substring match 4))
			    lily-unit-length))
	  (set-attribute 'text-anchor "start")
	  (set-attribute 'fill "currentColor"))
	(ly:warning (_ "cannot decypher Pango description: ~a") str))

    (apply entity 'text expr (reverse! alist))))

(define (font-smob-to-svg-font font expr)
  (let ((name-style (font-name-style font))
	(size (modified-font-metric-font-scaling font)))

    (entity 'text expr
	    ;; FIXME: The cdr of `name-style' cannot select the
	    ;; correct SVG font, so we ignore this information for now
	    `(font-family . ,(car name-style))
	    `(font-size . ,size)
	    '(text-anchor . "start"))))

(define (fontify font expr)
  (if (string? font)
      (pango-description-to-svg-font font expr)
      (font-smob-to-svg-font font expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stencil outputters
;;;

;;; catch-all for missing stuff
;;; comment this out to see find out what functions you miss :-)

(if #f
    (begin
      (define (dummy . foo) "")
      (map (lambda (x) (module-define! this-module x dummy))
	   (append
	    (ly:all-stencil-expressions)
	    (ly:all-output-backend-commands)))
      ))

(define (url-link url x y)
  (string-append
   (eo 'a `(xlink:href . ,url))
   (eoc 'rect
	`(x . ,(car x))
	`(y . ,(car y))
	`(width . ,(- (cdr x) (car x)))
	`(height . ,(- (cdr y) (car y)))
	'(fill . "none")
	'(stroke . "none")
	'(stroke-width . "0.0"))
   (ec 'a)))

(define (grob-cause offset grob)
  "")

(define (no-origin)
  "")



(define (bezier-sandwich lst thick)
  (let* ((first (list-tail lst 4))
	 (first-c0 (car (list-tail first 3)))
	 (second (list-head lst 4)))
    (entity 'path ""
	    '(stroke-linejoin . "round")
	    '(stroke-linecap . "round")
	    '(stroke . "currentColor")
	    '(fill . "currentColor")
	    `(stroke-width . ,thick)
	    `(d . ,(string-append (svg-bezier first #f)
				  (svg-bezier second first-c0)))
	    )))

(define (path thick commands)
  (define (convert-path-exps exps)
    (if (pair? exps)
	(let*
	    ((head (car exps))
	     (rest (cdr exps))
	     (arity 
	      (cond
	       ((memq head '(rmoveto rlineto lineto moveto)) 2)
	       ((memq head '(rcurveto curveto)) 6)
	       (else 1)))
	     (args (take rest arity))
	     (svg-head (assoc-get head '((rmoveto . m)
					 (rcurveto . c)
					 (curveto . C)
					 (moveto . M)
					 (lineto . L)
					 (rlineto . l))
				  ""))
	     )

	  (cons (format "~a~a "
			svg-head (number-list->point args)
			)
		(convert-path-exps (drop rest arity))))
	'()))
  
  (entity 'path ""
	  `(stroke-width . ,thick)
	  '(stroke-linejoin . "round")
	  '(stroke-linecap . "round")
	  '(stroke . "currentColor")
	  '(fill . "none")
	  `(d . ,(string-join (convert-path-exps commands) " "))))
  
(define (char font i)
  (dispatch
   `(fontify ,font ,(entity 'tspan (char->entity (integer->char i))))))

(define-public (comment s)
  (string-append "<!-- " s " !-->\n"))

(define (draw-line thick x1 y1 x2 y2 . alist)
  
  (apply entity 'line ""
	 (append
	  `((stroke-linejoin . "round")
	    (stroke-linecap . "round")
	    (stroke-width . ,thick)
	    (stroke . "currentColor")
	    (x1 . ,x1)
	    (y1 . ,(- y1))
	    (x2 . ,x2)
	    (y2 . ,(- y2)))
	  alist)))

(define (dashed-line thick on off dx dy phase)
  (draw-line thick 0 0 dx dy `(style . ,(format "stroke-dasharray:~a,~a;" on off))))

(define (named-glyph font name)
  (dispatch
   `(fontify ,font ,(entity 'tspan
			    (integer->entity
			     (ly:font-glyph-name-to-charcode font name))))))

(define (placebox x y expr)
  (let*
    ((match (regexp-exec svg-element-regexp expr))
     (tagname (match:substring match 1))
     (attributes (match:substring match 2)))

    (string-append tagname
		   ;; FIXME: Not using GNU coding standards
		   ;; [translate ()] here to work around a
		   ;; bug in Microsoft Internet Explorer 6.0
		   (ly:format " transform=\"translate(~f, ~f)\" " x (- y))
		   attributes
		   "\n")))

(define (polygon coords blot-diameter is-filled)
  (entity
   'polygon ""
   '(stroke-linejoin . "round")
   '(stroke-linecap . "round")
   `(stroke-width . ,blot-diameter)
   `(fill . ,(if is-filled "currentColor" "none"))
   '(stroke . "currentColor")
   `(points . ,(string-join
		(map offset->point (ly:list->offsets '() coords))))
   ))

;; rotate around given point
(define (setrotation ang x y)
  (format "<g transform=\"rotate(~a,~a,~a)\">\n"
    (number->string (* -1 ang))
    (number->string x)
    (number->string (* -1 y))))

(define (resetrotation ang x y)
  "</g>\n")

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
	  '(fill . "currentColor")
	  ))

(define (circle radius thick is-filled)
  (entity
   'circle ""
   '(stroke-linejoin . "round")
   '(stroke-linecap . "round")
   `(fill . ,(if is-filled "currentColor" "none"))
   `(stroke . "currentColor")
   `(stroke-width . ,thick)
   `(r . ,radius)))

(define (ellipse x-radius y-radius thick is-filled)
  (entity
   'ellipse ""
   '(stroke-linejoin . "round")
   '(stroke-linecap . "round")
   `(fill . ,(if is-filled "currentColor" "none"))
   `(stroke . "currentColor")
   `(stroke-width . ,thick)
   `(rx . ,x-radius)
   `(ry . ,y-radius)))

(define (oval x-radius y-radius thick is-filled)
  (let ((x-max x-radius)
        (x-min (- x-radius))
        (y-max y-radius)
        (y-min (- y-radius)))
    (entity
     'path ""
     '(stroke-linejoin . "round")
     '(stroke-linecap . "round")
     `(fill . ,(if is-filled "currentColor" "none"))
     `(stroke . "currentColor")
     `(stroke-width . ,thick)
     `(d . ,(ly:format "M~4f,~4f C~4f,~4f  ~4f,~4f ~4f,~4f S~4f,~4f ~4f,~4f" 
               x-max 0
               x-max y-max
               x-min y-max
               x-min 0
               x-max y-min
               x-max 0)))))

(define (text font string)
  (dispatch `(fontify ,font ,(entity 'tspan (string->entities string)))))

(define (utf-8-string pango-font-description string)
  (dispatch `(fontify ,pango-font-description ,(entity 'tspan string))))



(define (setcolor r g b)
  (format "<g color=\"rgb(~a%,~a%,~a%)\">\n"
	  (* 100 r) (* 100 g) (* 100 b)
	  ))

(define (resetcolor)
  "</g>\n")
