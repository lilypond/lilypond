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

(define lily-unit-length 1.7573)

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

(define-public (comment s)
  (string-append "<!-- " s " -->\n"))

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
  (make-regexp "^(<[a-z]+) ?(.*>)"))

(define scaled-element-regexp
  (make-regexp "^(<[a-z]+ transform=\")(scale.[-0-9. ]+,[-0-9. ]+.\" .*>)"))

(define pango-description-regexp-comma
  (make-regexp ",( Bold)?( Italic)?( Small-Caps)? ([0-9.]+)$"))

(define pango-description-regexp-nocomma
  (make-regexp "( Bold)?( Italic)?( Small-Caps)? ([0-9.]+)$"))

(define (pango-description-to-text str expr)
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

(define (dump-path path scale . rest)
  (define alist '())
  (define (set-attribute attr val)
    (set! alist (assoc-set! alist attr val)))
  (if (not (null? rest))
      (let* ((dx (car rest))
	     (dy (cadr rest))
	     (total-x (+ dx next-horiz-adv)))
	(if (or (not (= 0 (inexact->exact total-x)))
		(not (= 0 (inexact->exact dy))))
	    (let ((x (ly:format "~4f" total-x))
		  (y (ly:format "~4f" dy)))
	      (set-attribute 'transform
			     (string-append
			       "translate(" x ", " y ") "
			       "scale(" scale ", -" scale ")")))
	    (set-attribute 'transform
			   (string-append
			     "scale(" scale ", -" scale ")"))))
      (set-attribute 'transform (string-append
				  "scale(" scale ", -" scale ")")))

  (set-attribute 'd path)
  (apply entity 'path "" (reverse alist)))


;; A global variable for keeping track of the *cumulative*
;; horizontal advance for glyph strings, but only if there
;; is more than one glyph.
(define next-horiz-adv 0.0)

;; Matches the required "unicode" attribute from <glyph>
(define glyph-unicode-value-regexp
  (make-regexp "unicode=\"([^\"]+)\""))

;; Matches the optional path data from <glyph>
(define glyph-path-regexp
  (make-regexp "d=\"([-MmZzLlHhVvCcSsQqTt0-9.\n ]*)\""))

;; Matches a complete <glyph> element with the glyph-name
;; attribute value of NAME.  For example:
;;
;; <glyph glyph-name="period" unicode="." horiz-adv-x="110"
;; d="M0 55c0 30 25 55 55 55s55 -25 55
;; -55s-25 -55 -55 -55s-55 25 -55 55z" />
;;
;; TODO: it would be better to use an XML library to extract
;; the glyphs instead, and store them in a hash table.  --pmccarty
;;
(define (glyph-element-regexp name)
  (make-regexp (string-append "<glyph"
			      "(([\r\n\t ]+[-a-z]+=\"[^\"]*\")+)?"
			      "[\r\n\t ]+glyph-name=\"("
			      name
			      ")\""
			      "(([\r\n\t ]+[-a-z]+=\"[^\"]*\")+)?"
			      "([\r\n\t ]+)?"
			      "/>")))

(define (extract-glyph all-glyphs name size . rest)
  (let* ((new-name (regexp-quote name))
	 (regexp (regexp-exec (glyph-element-regexp new-name) all-glyphs))
	 (glyph (match:substring regexp))
	 (unicode-attr (regexp-exec glyph-unicode-value-regexp glyph))
	 (unicode-attr-value (match:substring unicode-attr 1))
	 (unicode-attr? (regexp-match? unicode-attr))
	 (d-attr (regexp-exec glyph-path-regexp glyph))
	 (d-attr-value "")
	 (d-attr? (regexp-match? d-attr))
	 ;; TODO: not urgent, but do not hardcode this value
	 (units-per-em 1000)
	 (font-scale (ly:format "~4f" (/ size units-per-em)))
	 (path ""))

    (if (and unicode-attr? (not unicode-attr-value))
	(ly:warning (_ "Glyph must have a unicode value")))

    (if d-attr? (set! d-attr-value (match:substring d-attr 1)))

    (cond (
	   ;; Glyph-strings with path data
	   (and d-attr? (not (null? rest)))
	   (begin
	     (set! path (apply dump-path d-attr-value
					 font-scale
					 (list (cadr rest) (caddr rest))))
	     (set! next-horiz-adv (+ next-horiz-adv
				     (car rest)))
	     path))
	  ;; Glyph-strings without path data ("space")
	  ((and (not d-attr?) (not (null? rest)))
	   (begin
	     (set! next-horiz-adv (+ next-horiz-adv
				     (car rest)))
	     ""))
	  ;; Font smobs with path data
	  ((and d-attr? (null? rest))
	    (set! path (dump-path d-attr-value font-scale))
	    path)
	  ;; Font smobs without path data ("space")
	  (else
	    ""))))

(define (extract-glyph-info all-glyphs glyph size)
  (let* ((offsets (list-head glyph 3))
	 (glyph-name (car (reverse glyph))))
    (apply extract-glyph all-glyphs glyph-name size offsets)))

(define (svg-defs svg-font)
  (let ((start (string-contains svg-font "<defs>"))
	(end (string-contains svg-font "</defs>")))
    (substring svg-font (+ start 7) (- end 1))))

(define (cache-font svg-font size glyph)
  (let ((all-glyphs (svg-defs (cached-file-contents svg-font))))
    (if (list? glyph)
	(extract-glyph-info all-glyphs glyph size)
	(extract-glyph all-glyphs glyph size))))


(define (feta-alphabet-to-path font size glyph)
  (let* ((name-style (font-name-style font))
	 (scaled-size (/ size lily-unit-length))
	 (font-file (ly:find-file (string-append name-style ".svg"))))

    (if font-file
	(cache-font font-file scaled-size glyph)
	(ly:warning (_ "cannot find SVG font ~S") font-file))))


(define (font-smob-to-path font glyph)
  (let* ((name-style (font-name-style font))
	 (scaled-size (modified-font-metric-font-scaling font))
	 (font-file (ly:find-file (string-append name-style ".svg"))))

    (if font-file
	(cache-font font-file scaled-size glyph)
	(ly:warning (_ "cannot find SVG font ~S") font-file))))


(define (fontify font expr)
  (if (string? font)
      (pango-description-to-text font expr)
      (font-smob-to-path font expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stencil outputters
;;;

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

(define (char font i)
  (dispatch
   `(fontify ,font ,(entity 'tspan (char->entity (integer->char i))))))

(define (circle radius thick is-filled)
  (entity
   'circle ""
   '(stroke-linejoin . "round")
   '(stroke-linecap . "round")
   `(fill . ,(if is-filled "currentColor" "none"))
   `(stroke . "currentColor")
   `(stroke-width . ,thick)
   `(r . ,radius)))

(define (dashed-line thick on off dx dy phase)
  (draw-line thick 0 0 dx dy `(style . ,(format "stroke-dasharray:~a,~a;" on off))))

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

(define (embedded-svg string)
  string)

(define (glyph-string font size cid glyphs)
  (define path "")
  (if (= 1 (length glyphs))
      (set! path (feta-alphabet-to-path font size (car glyphs)))
      (begin
	(set! path
	      (string-append (eo 'g)
			     (string-join
			       (map (lambda (x)
				      (feta-alphabet-to-path font size x))
				    glyphs)
			       "\n")
			     (ec 'g)))))
  (set! next-horiz-adv 0.0)
  path)

(define (grob-cause offset grob)
  "")

(define (named-glyph font name)
  (dispatch `(fontify ,font ,name)))

(define (no-origin)
  "")

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

(define (placebox x y expr)
  (if (string-null? expr)
      ""
      (let*
	((normal-element (regexp-exec svg-element-regexp expr))
	 (scaled-element (regexp-exec scaled-element-regexp expr))
	 (scaled? (if scaled-element #t #f))
	 (match (if scaled? scaled-element normal-element))
	 (string1 (match:substring match 1))
	 (string2 (match:substring match 2)))

	(if scaled?
	    (string-append string1
			   (ly:format "translate(~4f, ~4f) " x (- y))
			   string2
			   "\n")
	    (string-append string1
			   (ly:format " transform=\"translate(~4f, ~4f)\" "
				      x (- y))
			   string2
			   "\n")))))

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

(define (resetcolor)
  "</g>\n")

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

(define (setcolor r g b)
  (format "<g color=\"rgb(~a%,~a%,~a%)\">\n"
	  (* 100 r) (* 100 g) (* 100 b)
	  ))

;; rotate around given point
(define (setrotation ang x y)
  (format "<g transform=\"rotate(~a,~a,~a)\">\n"
    (number->string (* -1 ang))
    (number->string x)
    (number->string (* -1 y))))

(define (text font string)
  (dispatch `(fontify ,font ,(entity 'tspan (string->entities string)))))

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

(define (utf-8-string pango-font-description string)
  (dispatch `(fontify ,pango-font-description ,(entity 'tspan string))))
