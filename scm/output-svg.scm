;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2002--2022 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                Patrick McCarty <pnorcks@gmail.com>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

(define-module (lily output-svg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; globals

;;; set by framework-svg.scm
(define paper #f)

(define (set-paper p) (set! paper p))

(use-modules
 (guile)
 (ice-9 regex)
 (ice-9 format)
 (ice-9 optargs)
 (lily)
 (srfi srfi-1)
 (srfi srfi-13))

(define ice9-format format)
(define format simple-format)

(define lily-unit-length 1.7573)

;; Helper functions
(define-public (attributes attributes-alist)
  (string-concatenate
   (map (lambda (x)
          (let ((attr (car x))
                (value (cdr x)))
            (if (number? value)
                (set! value (ly:format "~4f" value)))
            (format #f " ~s=\"~a\"" attr value)))
        attributes-alist)))

(define-public (eo entity tNewline . attributes-alist)
  "o = open"
  (format #f "<~S~a>~a" entity
          (attributes attributes-alist)
          (if tNewline "\n" "")))

(define-public (eoc entity . attributes-alist)
  "oc = open/close"
  (format #f "<~S~a/>\n" entity (attributes attributes-alist)))

(define-public (ec entity)
  "c = close"
  (format #f "</~S>\n" entity))

(define (start-group-node attributes)
  (define attributes-string
    (string-concatenate
     (map (lambda (item)
            (ly:format " ~a=\"~a\"" (car item) (cdr item)))
          attributes)))
  (string-append "<g" attributes-string ">\n"))

(define-public (comment s)
  (if (string-null? s)
      ""
      (string-append "<!-- " s " -->\n")))

(define-public (entity entity string tNewline . attributes-alist)
  (if (string-null? string)
      (apply eoc entity attributes-alist)
      (string-append
       (apply eo entity tNewline attributes-alist) string (ec entity))))

(define (offset->point o)
  (ly:format "~4f ~4f" (car o) (- (cdr o))))

(define (number-list->point lst)
  (define (helper lst)
    (if (null? lst)
        '()
        (cons (ly:format "~4f ~4f" (car lst) (- (cadr lst)))
              (helper (cddr lst)))))

  (string-join (helper lst) " "))


(define (svg-bezier lst close)
  (let* ((c0 (car (list-tail lst 3)))
         (c123 (list-head lst 3)))
    (string-append
     (if (not close) "M" "L")
     (offset->point c0)
     "C" (string-join (map offset->point c123) " ")
     (if (not close) "" "z"))))

(define (sqr x)
  (* x x))

(define (integer->entity integer)
  (ice9-format "&#x~x;" integer))

(define (char->entity char)
  (integer->entity (char->integer char)))

(define (string->entities string)
  (string-concatenate
   (map char->entity (string->list string))))

(define pango-description-regexp-comma
  (make-regexp ",( Bold)?( Italic)?( Small-Caps)?[ -]([0-9.]+)$"))

(define pango-description-regexp-nocomma
  (make-regexp "( Bold)?( Italic)?( Small-Caps)?[ -]([0-9.]+)$"))

(define (pango-description-to-text str expr)
  (define alist '())
  (define (set-attribute attr val)
    (set! alist (assoc-set! alist attr val)))
  (let* ((match-1 (regexp-exec pango-description-regexp-comma str))
         (match-2 (regexp-exec pango-description-regexp-nocomma str))
         (match (if match-1 match-1 match-2)))

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
        (ly:warning (G_ "cannot decypher Pango description: ~a") str))

    (apply entity 'text expr #t (reverse! alist))))

(define (dump-path path scale . rest)
  (define alist '())
  (define (set-attribute attr val)
    (set! alist (assoc-set! alist attr val)))
  (if (not (null? rest))
      (let* ((dx (car rest))
             (dy (cadr rest))
             (total-x (+ dx next-horiz-adv)))
        (if (or (not (zero? total-x))
                (not (zero? dy)))
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
  (set-attribute 'fill "currentColor")
  (apply entity 'path "" #t (reverse alist)))


;; A global variable for keeping track of the *cumulative*
;; horizontal advance for glyph strings, but only if there
;; is more than one glyph.
(define next-horiz-adv 0.0)

;; Matches the required "unicode" attribute from <glyph>
(define glyph-unicode-value-regexp
  (make-regexp "unicode=\"([^\"]+)\""))

;; Matches the optional path data from <glyph>
(define glyph-path-regexp
  (make-regexp "d=\"([-+MmZzLlHhVvCcSsQqTtAa0-9,.Ee\n ]*)\""))

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
                              "(([[:space:]]+[-a-z]+=\"[^\"]*\")+)?"
                              "[[:space:]]+glyph-name=\"("
                              name
                              ")\""
                              "(([[:space:]]+[-a-z]+=\"[^\"]*\")+)?"
                              "([[:space:]]+)?"
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
        (ly:warning (G_ "Glyph must have a unicode value")))

    (if d-attr? (set! d-attr-value (match:substring d-attr 1)))

    (cond (
           ;; Glyph-strings with path data
           (and d-attr? (not (null? rest)))
           (begin
             (set! path (apply dump-path d-attr-value
                               font-scale
                               (list (caddr rest) (cadddr rest))))
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
  (let* ((offsets (list-head glyph 4))
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


(define (music-string-to-path font size glyph)
  (let* ((name-style (font-name-style font))
         (scaled-size (/ size lily-unit-length))
         (font-file (ly:find-file (string-append name-style ".svg"))))

    (if font-file
        (cache-font font-file scaled-size glyph)
        (ly:warning (G_ "cannot find SVG font ~S") font-file))))


(define (font-smob-to-path font glyph)
  (let* ((name-style (font-name-style font))
         (scaled-size (modified-font-metric-font-scaling font))
         (font-file (ly:find-file (string-append name-style ".svg"))))

    (if font-file
        (cache-font font-file scaled-size glyph)
        (ly:warning (G_ "cannot find SVG font ~S") font-file))))

(define (woff-font-smob-to-text font expr)
  (let* ((name-style (font-name-style font))
         (scaled-size (modified-font-metric-font-scaling font))
         (font-file (ly:find-file (string-append name-style ".woff")))
         (charcode (ly:font-glyph-name-to-charcode font expr))
         (char-lookup (format #f "&#~S;" charcode))
         (glyph-by-name (eoc 'altglyph `(glyphname . ,expr)))
         (apparently-broken
          (comment "FIXME: how to select glyph by name, altglyph is broken?"))
         (text (string-regexp-substitute "\n" ""
                                         (string-append glyph-by-name apparently-broken char-lookup))))
    (define alist '())
    (define (set-attribute attr val)
      (set! alist (assoc-set! alist attr val)))
    (set-attribute 'font-family name-style)
    (set-attribute 'font-size scaled-size)
    (apply entity 'text text #t (reverse! alist))))

(define font-smob-to-text
  (if (not (ly:get-option 'svg-woff))
      font-smob-to-path woff-font-smob-to-text))

(define (fontify font expr)
  (if (string? font)
      (pango-description-to-text font expr)
      (font-smob-to-text font expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stencil outputters
;;;

(define (circle radius thick is-filled)
  (entity
   'circle "" #f
   '(stroke-linejoin . "round")
   '(stroke-linecap . "round")
   `(fill . ,(if is-filled "currentColor" "none"))
   `(stroke . "currentColor")
   `(stroke-width . ,thick)
   `(r . ,radius)))

(define (dashed-line thick on off dx dy phase)
  (draw-line thick 0 0 dx dy
             `(stroke-dasharray . ,(format #f "~a,~a" on off))))

(define (draw-line thick x1 y1 x2 y2 . alist)
  (apply entity 'line "" #t
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
   'ellipse "" #t
   '(stroke-linejoin . "round")
   '(stroke-linecap . "round")
   `(fill . ,(if is-filled "currentColor" "none"))
   `(stroke . "currentColor")
   `(stroke-width . ,thick)
   `(rx . ,x-radius)
   `(ry . ,y-radius)))

(define (partial-ellipse x-radius y-radius start-angle end-angle thick connect fill)
  (define (make-ellipse-radius x-radius y-radius angle)
    (/ (* x-radius y-radius)
       (sqrt (+ (* (* y-radius y-radius)
                   (* (cos angle) (cos angle)))
                (* (* x-radius x-radius)
                   (* (sin angle) (sin angle)))))))
  (let*
      ((new-start-angle (* PI-OVER-180 (angle-0-360 start-angle)))
       (start-radius (make-ellipse-radius x-radius y-radius new-start-angle))
       (new-end-angle (* PI-OVER-180 (angle-0-360 end-angle)))
       (end-radius (make-ellipse-radius x-radius y-radius new-end-angle))
       (epsilon 1.5e-3)
       (x-end (- (* end-radius (cos new-end-angle))
                 (* start-radius (cos new-start-angle))))
       (y-end (- (* end-radius (sin new-end-angle))
                 (* start-radius (sin new-start-angle)))))
    (if (and (< (abs x-end) epsilon) (< (abs y-end) epsilon))
        (entity
         'ellipse "" #t
         `(fill . ,(if fill "currentColor" "none"))
         `(stroke . "currentColor")
         `(stroke-width . ,thick)
         '(stroke-linejoin . "round")
         '(stroke-linecap . "round")
         '(cx . 0)
         '(cy . 0)
         `(rx . ,x-radius)
         `(ry . ,y-radius))
        (entity
         'path "" #t
         `(fill . ,(if fill "currentColor" "none"))
         `(stroke . "currentColor")
         `(stroke-width . ,thick)
         '(stroke-linejoin . "round")
         '(stroke-linecap . "round")
         (cons
          'd
          (string-append
           (ly:format
            "M~4f ~4fA~4f ~4f 0 ~4f 0 ~4f ~4f"
            (* start-radius (cos new-start-angle))
            (- (* start-radius (sin new-start-angle)))
            x-radius
            y-radius
            (if (> 0 (- new-start-angle new-end-angle)) 0 1)
            (* end-radius (cos new-end-angle))
            (- (* end-radius (sin new-end-angle))))
           (if connect
               (ly:format "L~4f,~4f"
                          (* start-radius (cos new-start-angle))
                          (- (* start-radius (sin new-start-angle))))
               "")))))))

(define (embedded-svg string)
  string)

(define (embedded-glyph-string pango-font font size cid glyphs
                               xfile-name face-index text clusters)
  (define path "")
  (if (= 1 (length glyphs))
      (set! path (music-string-to-path font size (car glyphs)))
      (begin
        (set! path
              (string-append (eo 'g #t)
                             (string-join
                              (map (lambda (x)
                                     (music-string-to-path font size x))
                                   glyphs)
                              "\n")
                             (ec 'g)))))
  (set! next-horiz-adv 0.0)
  path)

(define (woff-glyph-string pango-font font-name size cid?
                           w-h-x-y-named-glyphs file-name face-index
                           text clusters)
  (let* ((name-style (font-name-style font-name))
         (family-designsize (regexp-exec (make-regexp "(.*)-([0-9]*)")
                                         font-name))
         (family (if (regexp-match? family-designsize)
                     (match:substring family-designsize 1)
                     font-name))
         (design-size (if (regexp-match? family-designsize)
                          (match:substring family-designsize 2)
                          #f))
         (scaled-size (/ size lily-unit-length))
         (font (ly:paper-get-font paper `(((font-family . ,family)
                                           ,(if design-size
                                                `(design-size . design-size)))))))
    (define (glyph-spec w h x y g) ; h not used
      (let* ((charcode (ly:font-glyph-name-to-charcode font g))
             (char-lookup (format #f "&#~S;" charcode))
             (glyph-by-name (eoc 'altglyph `(glyphname . ,g)))
             (apparently-broken
              (comment "XFIXME: how to select glyph by name, altglyph is broken?")))
        ;; what is W?
        (ly:format
         "<text~a font-family=\"~a\" font-size=\"~a\">~a</text>"
         (if (or (> (abs x) 0.00001)
                 (> (abs y) 0.00001))
             (ly:format " transform=\"translate(~4f,~4f)\"" x y)
             " ")
         name-style scaled-size
         (string-regexp-substitute
          "\n" ""
          (string-append glyph-by-name apparently-broken char-lookup)))))

    (string-join (map (lambda (x) (apply glyph-spec x))
                      (reverse w-h-x-y-named-glyphs)) "\n")))

(define glyph-string
  (if (not (ly:get-option 'svg-woff)) embedded-glyph-string woff-glyph-string))

(define have-grob-cause? #f)



(define (grob-cause offset grob)
  (define (to-string x)
    (if (string? x)
        x
        ""))

  (to-string
   (if (ly:get-option 'point-and-click)
       (let* ((cause (ly:grob-property grob 'cause))
              (music-origin (if (ly:stream-event? cause)
                                (ly:event-property cause 'origin)))
              (point-and-click (ly:get-option 'point-and-click)))
         (and (ly:input-location? music-origin)
              (cond ((boolean? point-and-click) point-and-click)
                    ((symbol? point-and-click)
                     (ly:in-event-class? cause point-and-click))
                    (else (any (lambda (t)
                                 (ly:in-event-class? cause t))
                               point-and-click)))
              (let* ((location (ly:input-file-line-char-column music-origin))
                     (raw-file (car location))
                     (file (if (is-absolute? raw-file)
                               raw-file
                               (string-append (ly-getcwd) "/" raw-file))))

                (set! have-grob-cause? #t)
                (format #f "<a style=\"color:inherit;\" xlink:href=\"textedit://~a:~a:~a:~a\">\n"
                        ;; Backslashes are not valid
                        ;; file URI path separators.
                        (ly:string-percent-encode
                         (ly:string-substitute "\\" "/" file))

                        (cadr location)
                        (caddr location)
                        (1+ (cadddr location))))))
       "")))

(define (no-origin)
  (if have-grob-cause?
      (begin
        (set! have-grob-cause? #f)
        "</a>\n")
      ""))

(define (named-glyph font name)
  (fontify font name))


(define* (path thick commands #:optional (cap 'round) (join 'round) (fill? #f))
  (define (convert-path-exps exps)
    (if (pair? exps)
        (let*
            ((head (car exps))
             (rest (cdr exps))
             (arity
              (cond ((memq head '(rmoveto rlineto lineto moveto)) 2)
                    ((memq head '(rcurveto curveto)) 6)
                    ((eq? head 'closepath) 0)
                    (else 1)))
             (args (take rest arity))
             (svg-head (assoc-get head
                                  '((rmoveto . m)
                                    (rcurveto . c)
                                    (curveto . C)
                                    (moveto . M)
                                    (lineto . L)
                                    (rlineto . l)
                                    (closepath . z))
                                  "")))

          (cons (format #f "~a~a" svg-head (number-list->point args))
                (convert-path-exps (drop rest arity))))
        '()))

  (let* ((line-cap-styles '(butt round square))
         (line-join-styles '(miter round bevel))
         (cap-style (if (not (memv cap line-cap-styles))
                        (begin
                          (ly:warning (G_ "unknown line-cap-style: ~S")
                                      (symbol->string cap))
                          'round)
                        cap))
         (join-style (if (not (memv join line-join-styles))
                         (begin
                           (ly:warning (G_ "unknown line-join-style: ~S")
                                       (symbol->string join))
                           'round)
                         join)))
    (entity 'path "" #t
            `(stroke-width . ,thick)
            `(stroke-linejoin . ,(symbol->string join-style))
            `(stroke-linecap . ,(symbol->string cap-style))
            '(stroke . "currentColor")
            `(fill . ,(if fill? "currentColor" "none"))
            `(d . ,(string-concatenate (convert-path-exps commands))))))


(define (polygon coords blot-diameter is-filled)
  (entity
   'polygon "" #t
   '(stroke-linejoin . "round")
   '(stroke-linecap . "round")
   `(stroke-width . ,blot-diameter)
   `(fill . ,(if is-filled "currentColor" "none"))
   '(stroke . "currentColor")
   `(points . ,(string-join
                (map offset->point (ly:list->offsets '() coords))))))

(define (end-group . args)
  "</g>\n")

(define (round-filled-box breapth width depth height blot-diameter)
  (entity
   'rect "" #t
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
   '(fill . "currentColor")))

(define* (setcolor r g b #:optional (a #f))
  (string-append
   "<g color=\""
   (if (and (number? a) (< a 1.0))
       (ly:format "rgba(~4f%, ~4f%, ~4f%, ~4f%)"
                  (* 100 r) (* 100 g) (* 100 b) (* 100 a))
       (ly:format "rgb(~4f%, ~4f%, ~4f%)"
                  (* 100 r) (* 100 g) (* 100 b)))
   "\">\n"))

;; rotate around given point
(define (setrotation ang x y)
  (ly:format "<g transform=\"rotate(~4f, ~4f, ~4f)\">\n"
             (- ang) x (- y)))

(define (setscale x y)
  (ly:format "<g transform=\"scale(~4f, ~4f)\">\n"
             x y))

(define (settranslation x y)
  (ly:format "<g transform=\"translate(~4f, ~4f)\">\n" x (- y)))

(define (text font string)
  (fontify font (entity 'tspan (string->entities string) #f)))

(define (url-link url x y)
  (string-append
   (eo 'a #t `(xlink:href . ,url))
   (eoc 'rect
        `(x . ,(car x))
        `(y . ,(car y))
        `(width . ,(- (cdr x) (car x)))
        `(height . ,(- (cdr y) (car y)))
        '(fill . "none")
        '(stroke . "none")
        '(stroke-width . "0.0"))
   (ec 'a)))

(define (utf-8-string pango-font-description string orig)
  (let ((escaped-string (string-regexp-substitute
                         "<" "&lt;"
                         (string-regexp-substitute "&" "&amp;" string))))
    (fontify pango-font-description
             (entity 'tspan escaped-string #f))))

(define (set-unit-length len)
  (set! lily-unit-length len)
  "")

(define-public stencil-dispatch-alist
  `((circle . ,circle)
    (start-group-node . ,start-group-node)
    (end-group-node . ,end-group)
    (dashed-line . ,dashed-line)
    (draw-line . ,draw-line)
    (partial-ellipse . ,partial-ellipse)
    (ellipse . ,ellipse)
    (glyph-string . ,glyph-string)
    (grob-cause . ,grob-cause)
    (named-glyph . ,named-glyph)
    (no-origin . ,no-origin)
    (settranslation . ,settranslation)
    (resettranslation . ,end-group)
    (polygon . ,polygon)
    (round-filled-box . ,round-filled-box)
    (setcolor . ,setcolor)
    (resetcolor . ,end-group)
    (setrotation . ,setrotation)
    (resetrotation . ,end-group)
    (url-link . ,url-link)
    (utf-8-string . ,utf-8-string)
    (path . ,path)
    (setscale . ,setscale)
    (set-unit-length . ,set-unit-length)
    (resetscale . ,end-group)))
