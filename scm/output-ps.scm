;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define (eps-file file-name contents bbox factor)
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

    (string-append
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
     contents
     "%%EndDocument
EndEPSF
grestore
")))

(define (glyph-string pango-font
                      postscript-font-name
                      size
                      cid?
                      w-x-y-named-glyphs
                      file-name
                      face-index
                      text clusters)
  (define (glyph-spec w h x y g) ; h not used
    (let ((prefix (if (string? g) "/" "")))
      (ly:format "~4f ~4f ~4f ~a~a" w x y prefix g)))
  (define (emglyph-spec w h x y g) ; h not used
    (if (and (= x 0) (= y 0))
        (ly:format "currentpoint ~a moveto ~4f 0 rmoveto" g w)
        (ly:format "currentpoint ~4f ~4f rmoveto ~a moveto ~4f 0 rmoveto" x y g w)))
  (if cid?
      (ly:format
       "/~a /CIDFont findresource ~a output-scale div scalefont setfont\n~a\n~a print_glyphs\n"
       postscript-font-name size
       (string-join (map (lambda (x) (apply glyph-spec x))
                         (reverse w-x-y-named-glyphs)) "\n")
       (length w-x-y-named-glyphs))
      (if (and (ly:get-option 'music-font-encodings)
               (string-startswith postscript-font-name "Emmentaler"))
          ;; The 'P' font encoding is for communication with Pango.
          (ly:format "/~a-P ~a output-scale div selectfont\n~a\n"
                     postscript-font-name size
                     (string-join (map (lambda (x) (apply emglyph-spec x))
                                       w-x-y-named-glyphs) "\n"))
          (ly:format "/~a ~a output-scale div selectfont\n~a\n~a print_glyphs\n"
                     postscript-font-name size
                     (string-join (map (lambda (x)
                                         (apply glyph-spec x))
                                       (reverse w-x-y-named-glyphs)) "\n")
                     (length w-x-y-named-glyphs)))))

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
(define* (setcolor r g b #:optional (a #f))
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
