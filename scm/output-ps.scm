;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2023 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

;;;; Note: currently misused as testbed for titles with markup, see
;;;;       input/test/title-markup.ly
;;;;
;;;; TODO:
;;;;   * %% Papersize in (header ...)
;;;;   * text setting, kerning.
;;;;   * document output-interface

(define-module (lily output-ps))

(use-modules (guile)
             (ice-9 optargs)
             (srfi srfi-1)
             (srfi srfi-13)
             (ice-9 match)
             (ice-9 textual-ports)
             (lily framework-ps)
             (lily))

;;;
;;; Lily output interface, PostScript implementation --- cleanup and docme
;;;

(define (circle radius thick fill)
  (ly:format
   "~a ~4f ~4f draw_circle\n"
   (if fill
       "true"
       "false")
   radius thick))

(define (dashed-line thick on off dx dy phase)
  (ly:format "~4f ~4f ~4f [ ~4f ~4f ] ~4f draw_dashed_line\n"
             dx
             dy
             thick
             on
             off
             phase))

(define (draw-line thick x1 y1 x2 y2)
  (ly:format "~4f ~4f ~4f ~4f ~4f draw_line\n"
             (- x2 x1) (- y2 y1)
             x1 y1 thick))

(define (partial-ellipse x-radius y-radius start-angle end-angle thick connect fill)
  (ly:format "~a ~a ~4f ~4f ~4f ~4f ~4f draw_partial_ellipse\n"
             (if fill "true" "false")
             (if connect "true" "false")
             x-radius
             y-radius
             start-angle
             end-angle
             thick))

(define (ellipse x-radius y-radius thick fill)
  (ly:format
   "~a ~4f ~4f ~4f draw_ellipse\n"
   (if fill
       "true"
       "false")
   x-radius y-radius thick))

(define (embedded-ps string)
  string)

;; Auxiliary routine for eps-file and png-file.  contents-outputter is a
;; procedure outputting the EPS file contents to a port.
(define (dump-eps file-name contents-outputter bbox factor)
  (let*
      ;; We need to shift the whole eps to (0,0), otherwise it will appear
      ;; displaced in lilypond (displacement will depend on the scaling!)
      ((translate-string (ly:format "~a ~a translate" (- (list-ref bbox 0)) (- (list-ref bbox 1))))
       (clip-rect-string (ly:format
                          "~a ~a ~a ~a rectclip"
                          (list-ref bbox 0)
                          (list-ref bbox 1)
                          (- (list-ref bbox 2) (list-ref bbox 0))
                          (- (list-ref bbox 3) (list-ref bbox 1)))))
    ;; Return a bytevector here: when we go through this function for outputting
    ;; a PNG file, the EPS data (converted from PNG through Cairo) can be very
    ;; bulky.  Experimentally, this leads to a huge cost for decoding the string
    ;; from bytes and then reencoding it.
    (call-with-output-bytevector
     (lambda (port)
       (set-port-encoding! port "latin1")
       (display
        (ly:format
         "gsave
currentpoint translate
BeginEPSF
~a dup scale
~a
~a
%%BeginDocument: ~a
"
         factor translate-string  clip-rect-string file-name)
        port)
       (contents-outputter port)
       (display
        "%%EndDocument
EndEPSF
grestore
"
        port)))))

(define (eps-file file-name contents bbox factor)
  (let ((contents-outputter (lambda (port)
                              (put-string port contents))))
    (dump-eps file-name contents-outputter bbox factor)))

(define (png-file file-name width height factor background-color)
  (if background-color
      (let* ((contents-outputter
              (lambda (port)
                (apply ly:png->eps-dump file-name port background-color))))
        (dump-eps file-name
                  contents-outputter
                  (list 0 0 width height)
                  factor))
      (begin
        (ly:warning
         (G_ "transparency (no background color) for PNG images is not
supported in the PostScript backend.  Use the Cairo backend instead."))
        "")))

(define (glyph-string pango-font
                      postscript-font-name
                      size
                      cid?
                      w-hd-x-y-g-gn
                      file-name
                      face-index
                      text
                      clusters)
  "Emit a series of glyphs using a given font.  *pango-font* holds a pointer to
the glyphs' associated `Pango_font` structure, *postscript-font-name* is the
font's PostScript name, *size* is the font's size, and *cid?* is a Boolean flag
that, if set, indicates whether glyphs are specified by CID values instead of
glyph names (this is only relevant for CID-keyed OpenType Fonts).

*w-hd-x-y-g-gn* is a list of sublists, where each sublist describes a single
glyph and has the form

```
(width (height . depth) x-offset y-offset glyph-index glyph-name-or-CID-value)
```

*width*, *height*, and *depth* are the advance width, height, and depth of a
glyph, respectively; *x-offset* and *y-offset* are the horizontal and vertical
offsets relative to a glyph's current position, respectively, where the glyph
must be drawn.  *glyph-index* holds the index, and *glyph-name-or-CID-value*
holds the corresponding glyph name (as a string) or CID value (as an integer)
depending on *cid?*.

*file-name* is file name of the font; *face-index* is a value returned by the
Fontconfig library to access subfonts and/or named instances in the font.

*text* holds the original input text for the glyphs, and *clusters* is a list of
pairs of the form `(num-bytes . num-glyphs)` for mapping input characters to
output glyphs as described by the Cairo structure `cairo_text_cluster_t`."
  (define (glyph-spec w hd x y g gn) ; `hd` and `g` are not used
    (let ((prefix (if (string? gn) "/" "")))
      (ly:format "~4f ~4f ~4f ~a~a" w x y prefix gn)))
  (define (emglyph-spec w hd x y g gn) ; `hd` and `g` are not used
    (if (and (= x 0) (= y 0))
        (ly:format "currentpoint ~a moveto ~4f 0 rmoveto" gn w)
        (ly:format "currentpoint ~4f ~4f rmoveto ~a moveto ~4f 0 rmoveto"
                   x y gn w)))
  (if cid?
      (ly:format
       "/~a /CIDFont findresource ~a output-scale div scalefont setfont\n~a\n~a print_glyphs\n"
       postscript-font-name size
       (string-join (map (lambda (x) (apply glyph-spec x))
                         (reverse w-hd-x-y-g-gn))
                    "\n")
       (length w-hd-x-y-g-gn))
      (if (and (ly:get-option 'music-font-encodings)
               (string-startswith postscript-font-name "Emmentaler"))
          ;; The 'P' font encoding is for communication with Pango.
          (ly:format "/~a-P ~a output-scale div selectfont\n~a\n"
                     postscript-font-name size
                     (string-join (map (lambda (x) (apply emglyph-spec x))
                                       w-hd-x-y-g-gn)
                                  "\n"))
          (ly:format "/~a ~a output-scale div selectfont\n~a\n~a print_glyphs\n"
                     postscript-font-name size
                     (string-join (map (lambda (x)
                                         (apply glyph-spec x))
                                       (reverse w-hd-x-y-g-gn))
                                  "\n")
                     (length w-hd-x-y-g-gn)))))

(define (grob-cause offset grob)
  (if (ly:get-option 'point-and-click)
      (let* ((cause (ly:grob-property grob 'cause))
             (music-origin (if (ly:stream-event? cause)
                               (ly:event-property cause 'origin)))
             (point-and-click (ly:get-option 'point-and-click)))
        (if (and
             (ly:input-location? music-origin)
             (cond ((boolean? point-and-click) point-and-click)
                   ((symbol? point-and-click)
                    (ly:in-event-class? cause point-and-click))
                   (else (any (lambda (t)
                                (ly:in-event-class? cause t))
                              point-and-click))))
            (let* ((location (ly:input-file-line-char-column music-origin))
                   (raw-file (car location))
                   (file (if (is-absolute? raw-file)
                             raw-file
                             (string-append (ly-getcwd) "/" raw-file)))
                   (x-ext (ly:grob-extent grob grob X))
                   (y-ext (ly:grob-extent grob grob Y)))

              (if (and (< 0 (interval-length x-ext))
                       (< 0 (interval-length y-ext)))
                  (ly:format " ~4f ~4f ~4f ~4f (textedit://~a:~a:~a:~a) mark_URI\n"
                             (+ (car offset) (car x-ext))
                             (+ (cdr offset) (car y-ext))
                             (+ (car offset) (cdr x-ext))
                             (+ (cdr offset) (cdr y-ext))

                             ;; Backslashes are not valid
                             ;; file URI path separators.
                             (ly:string-percent-encode
                              (ly:string-substitute "\\" "/" file))

                             (cadr location)
                             (caddr location)
                             (1+ (cadddr location)))
                  ""))
            ""))
      ""))

(define (named-glyph font glyph)
  (if (and (ly:get-option 'music-font-encodings)
           (string-startswith (ly:font-file-name font) "emmentaler"))
      (format #f "~a-~a ~a\n"
              (ps-font-command font)
              (hash-ref
               (if (string-endswith (ly:font-file-name font) "-brace")
                   brace-encoding-table
                   glyph-encoding-table) glyph)
              glyph)
      (format #f "~a /~a glyphshow\n" (ps-font-command font) glyph)))

(define (no-origin)
  "")

(define (settranslation x y)
  (ly:format " ~4f ~4f moveto\n" x y))

(define (polygon points blot-diameter filled?)
  (ly:format "~a ~a ~a ~4f draw_polygon\n"
             (if filled? "true" "false")
             (string-join (map (lambda (p) (ly:format "~4f" p)) points) " ")
             (- (/ (length points) 2) 1)
             blot-diameter))

(define (round-filled-box left right bottom top blotdiam)
  (let* ((halfblot (/ blotdiam 2))
         (x (- halfblot left))
         (width (- right (+ halfblot x)))
         (y (- halfblot bottom))
         (height (- top (+ halfblot y))))
    (ly:format "~4f ~4f ~4f ~4f ~4f draw_round_box\n"
               width height x y blotdiam)))

;; save current color on stack and set new color
(define (setcolor r g b a)
  ;; TODO: figure out a way to support alpha transparency
  ;; using /SetTransparency pdfmark
  (ly:format "gsave ~4f ~4f ~4f setrgbcolor\n" r g b))

;; restore color from stack
(define (resetcolor) "grestore\n")

;; rotation around given point
(define (setrotation ang x y)
  (ly:format "gsave ~4f ~4f translate ~a rotate ~4f ~4f translate\n"
             x y ang (- x) (- y)))

(define (resetrotation ang x y)
  "grestore\n")

(define (url-link url x y)
  (ly:format "~a ~a currentpoint vector_add  ~a ~a currentpoint vector_add (~a) mark_URI\n"
             (car x)
             (car y)
             (cdr x)
             (cdr y)
             url))

(define (page-link page-no x y)
  (if (number? page-no)
      (ly:format "~a ~a currentpoint vector_add  ~a ~a currentpoint vector_add ~a mark_page_link\n"
                 (car x)
                 (car y)
                 (cdr x)
                 (cdr y)
                 page-no)
      ""))

(define* (path thickness exps #:optional (cap 'round) (join 'round) (fill? #f))
  (define (convert-path-exps exps)
    (if (pair? exps)
        (let*
            ((head (car exps))
             (rest (cdr exps))
             (arity
              (cond
               ((memq head '(rmoveto rlineto lineto moveto)) 2)
               ((memq head '(rcurveto curveto)) 6)
               ((eq? head 'closepath) 0)
               (else 1)))
             (args (take rest arity))
             )

          ;; WARNING: this is a vulnerability: a user can output arbitrary PS code here.
          (append
           (map (lambda (a) (ly:format "~a" a)) args)
           (list (symbol->string head))
           (convert-path-exps (drop rest arity))))
        '()))

  (let ((cap-numeric (case cap ((butt) 0) ((round) 1) ((square) 2)
                           (else (begin
                                   (ly:warning (G_ "unknown line-cap-style: ~S")
                                               cap)
                                   1))))
        (join-numeric (case join ((miter) 0) ((round) 1) ((bevel) 2)
                            (else (begin
                                    (ly:warning (G_ "unknown line-join-style: ~S")
                                                (symbol->string join))
                                    1)))))
    (ly:format
     "gsave currentpoint translate
~a setlinecap ~a setlinejoin ~a setlinewidth
~a ~a grestore\n"
     cap-numeric
     join-numeric
     thickness
     (string-join (convert-path-exps exps) " ")
     ;; print outline contour only if there is no fill or if
     ;; contour is explicitly requested with a thickness > 0
     (cond ((not fill?) "stroke")
           ((positive? thickness) "gsave stroke grestore fill")
           (else "fill")))))

(define (setscale x y)
  (ly:format "gsave ~4f ~4f scale\n" x y))

(define (resetscale)
  "grestore\n")

(define-public stencil-dispatch-alist
  `((circle . ,circle)
    (dashed-line . ,dashed-line)
    (draw-line . ,draw-line)
    (eps-file . ,eps-file)
    (partial-ellipse . ,partial-ellipse)
    (ellipse . ,ellipse)
    (embedded-ps . ,embedded-ps)
    (glyph-string . ,glyph-string)
    (grob-cause . ,grob-cause)
    (named-glyph . ,named-glyph)
    (no-origin . ,no-origin)
    (png-file . ,png-file)
    (settranslation . ,settranslation)
    (polygon . ,polygon)
    (round-filled-box . ,round-filled-box)
    (setcolor . ,setcolor)
    (resetcolor . ,resetcolor)
    (setrotation . ,setrotation)
    (resetrotation . ,resetrotation)
    (reset-grob-cause . ,no-origin)
    (url-link . ,url-link)
    (page-link . ,page-link)
    (path . ,path)
    (setscale . ,setscale)
    (resetscale . ,resetscale)))
