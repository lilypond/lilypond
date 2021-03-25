;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2009--2020 Marc Hohl <marc@hohlart.de>
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



;; TODO:
;; (1) Dashed bar lines may stick out above and below the staff lines
;;
;; (2) Dashed and dotted lines look ugly in combination with span bars
;;
;; (This was the case in the c++-version of (span) bar stuff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions for staff and layout properties

(define (bar-line::calc-blot thickness extent grob)
  "Calculate the blot diameter by taking @code{'rounded}
and the dimensions of the extent into account."
  (let* ((rounded (ly:grob-property grob 'rounded #f))
         (blot (if rounded
                   (let ((blot-diameter (layout-blot-diameter grob))
                         (height (interval-length extent)))

                     (cond ((< thickness blot-diameter) thickness)
                           ((< height blot-diameter) height)
                           (else blot-diameter)))
                   0)))
    blot))

(define-public (bar-line::draw-filled-box x-ext y-ext thickness extent grob)
  "Return a straight bar-line created by @code{ly:round-filled-box} looking at
@var{x-ext}, @var{y-ext}, @var{thickness}.  The blot is calculated by
@code{bar-line::calc-blot}, which needs @var{extent} and @var{grob}.
@var{y-ext} is not necessarily of same value as @var{extent}."
  (ly:round-filled-box
   x-ext
   y-ext
   (bar-line::calc-blot thickness extent grob)))

(define (get-span-glyph bar-glyph)
  "Get the corresponding span glyph from the @code{span-glyph-bar-alist}.
Pad the string with @code{annotation-char}s to the length of the
@var{bar-glyph} string."
  (let ((span-glyph (assoc-get bar-glyph span-bar-glyph-alist bar-glyph)))

    (if (string? span-glyph)
        (set! span-glyph (string-pad-right
                          span-glyph
                          (string-length bar-glyph)
                          replacement-char)))
    span-glyph))

(define (get-staff-symbol grob)
  "Return the staff symbol corresponding to Grob @var{grob}."
  (if (grob::has-interface grob 'staff-symbol-interface)
      grob
      (ly:grob-object grob 'staff-symbol)))

(define (layout-blot-diameter grob)
  "Get the blot diameter of the @var{grob}'s corresponding layout."
  (let* ((layout (ly:grob-layout grob))
         (blot-diameter (ly:output-def-lookup layout 'blot-diameter)))

    blot-diameter))

(define (staff-symbol-line-count staff)
  "Get or compute the number of lines of staff @var{staff}."
  (let ((line-count 0))

    (if (ly:grob? staff)
        (let ((line-pos (ly:grob-property staff 'line-positions '())))

          (set! line-count (if (pair? line-pos)
                               (length line-pos)
                               (ly:grob-property staff 'line-count 0)))))

    line-count))

(define (staff-symbol-line-span grob)
  (let ((line-pos (ly:grob-property grob 'line-positions '()))
        (iv (cons 0.0 0.0)))

    (if (pair? line-pos)
        (begin
          (set! iv (cons (car line-pos) (car line-pos)))
          (for-each (lambda (x)
                      (set! iv (cons (min (car iv) x)
                                     (max (cdr iv) x))))
                    (cdr line-pos)))

        (let ((line-count (ly:grob-property grob 'line-count 0)))

          (set! iv (cons (- 1 line-count)
                         (- line-count 1)))))
    iv))

(define (staff-symbol-line-positions grob)
  "Get or compute the @code{'line-positions} list from @var{grob}."
  (let ((line-pos (ly:grob-property grob 'line-positions '())))

    (if (not (pair? line-pos))
        (let* ((line-count (ly:grob-property grob 'line-count 0))
               (height (- line-count 1.0)))

          (set! line-pos (map (lambda (x)
                                (- height (* x 2)))
                              (iota line-count)))))
    line-pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal helper functions

(define annotation-char #\-)
(define replacement-char #\ )

(define dummy-extent (cons -1 1))


(define (glyph->stencil glyph grob extent)
  "Return a stencil computed by the procedure associated with
glyph @var{glyph}. The arguments @var{grob} and @var{extent} are
mandatory to the procedures stored in @code{bar-glyph-print-procedures}."
  (let ((proc (assoc-get glyph bar-glyph-print-procedures))
        (stencil empty-stencil))

    (if (procedure? proc)
        (set! stencil (proc grob extent))
        (ly:warning (_ "Bar glyph ~a not known. Ignoring.") glyph))
    stencil))

(define (string->string-list str)
  "Convert a string into a list of strings with length 1.
@code{\"aBc\"} will be converted to @code{(\"a\" \"B\" \"c\")}.
An empty string will be converted to a list containing @code{\"\"}."
  (if (and (string? str)
           (not (zero? (string-length str))))
      (map (lambda (s)
             (string s))
           (string->list str))
      (list "")))

(define (strip-string-annotation str)
  "Strip annotations starting with and including the
annotation char from string @var{str}."
  (let ((pos (string-index str annotation-char)))

    (if pos
        (substring str 0 pos)
        str)))

(define (check-for-annotation str)
  "Check whether the annotation char is present in string @var{str}."
  (if (string? str)
      (if (string-index str annotation-char)
          (ly:warning
           (_ "Annotation '~a' is allowed in the first argument of a bar line definition only.")
           str))))

(define (check-for-replacement str)
  "Check whether the replacement char is present in string @var{str}."
  (if (string? str)
      (if (string-index str replacement-char)
          (ly:warning
           (_ "Replacement '~a' is allowed in the last argument of a bar line definition only.")
           str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line break decisions.

(define-public (define-bar-line bar-glyph eol-glyph bol-glyph span-glyph)
  "Define a bar glyph @var{bar-glyph} and its substitute at the end of
a line (@var{eol-glyph}), at the beginning of a new line (@var{bol-glyph})
and as a span bar (@var{span-glyph}) respectively."
  ;; the last argument may not include annotations
  (check-for-annotation span-glyph)
  ;; only the last argument may call for replacements
  (for-each (lambda (s)
              (check-for-replacement s))
            (list bar-glyph eol-glyph bol-glyph))
  ;; the bar-glyph-alist has entries like
  ;; (bar-glyph . ( eol-glyph . bol-glyph))
  (set! bar-glyph-alist
        (acons bar-glyph (cons eol-glyph bol-glyph) bar-glyph-alist))

  ;; the span-bar-glyph-alist has entries like
  ;; (bar-glyph . span-glyph)
  (set! span-bar-glyph-alist
        (acons bar-glyph span-glyph span-bar-glyph-alist)))

(define-session bar-glyph-alist '())

(define-session span-bar-glyph-alist '())

(define-public (add-bar-glyph-print-procedure glyph proc)
  "Specify the single glyph @var{glyph} that calls print procedure @var{proc}.
The procedure @var{proc} has to be defined in the form
@code{(make-...-bar-line grob extent)} even if the @var{extent}
is not used within the routine."
  (if (or (not (string? glyph))
          (> (string-length glyph) 1))
      (ly:warning
       (_ "add-bar-glyph-print-procedure: glyph '~a' has to be a single ASCII character.")
       glyph)
      (set! bar-glyph-print-procedures
            (acons glyph proc bar-glyph-print-procedures))))

(define-session bar-glyph-print-procedures `())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; drawing functions for various bar line types
;; to include them and other user-defined functions,
;; all of them have the form
;; (make-...-bar-line grob extent)
;; even if the extent is not used.

(define (make-empty-bar-line grob extent)
  "Draw an empty bar line."
  (ly:make-stencil "" (cons 0 0) extent))

(define (make-simple-bar-line grob extent)
  "Draw a simple bar line."
  (let* ((line-thickness (layout-line-thickness grob))
         (thickness (* (ly:grob-property grob 'hair-thickness 1)
                       line-thickness))
         (extent (bar-line::widen-bar-extent-on-span grob extent)))
    (bar-line::draw-filled-box
     (cons 0 thickness)
     extent
     thickness
     extent
     grob)))

(define (make-thick-bar-line grob extent)
  "Draw a thick bar line."
  (let* ((line-thickness (layout-line-thickness grob))
         (thickness (* (ly:grob-property grob 'thick-thickness 1)
                       line-thickness))
         (extent (bar-line::widen-bar-extent-on-span grob extent)))
    (bar-line::draw-filled-box
     (cons 0 thickness)
     extent
     thickness
     extent
     grob)))

(define (make-tick-bar-line grob extent)
  "Draw a tick bar line."
  (let* ((half-staff (* 1/2 (ly:staff-symbol-staff-space grob)))
         (staff-line-thickness (ly:staff-symbol-line-thickness grob))
         (height (interval-end extent)))
    (bar-line::draw-filled-box
     (cons 0 staff-line-thickness)
     (cons (- height half-staff) (+ height half-staff))
     staff-line-thickness
     extent
     grob)))

(define (make-colon-bar-line grob extent)
  "Draw repeat dots."
  (let* ((staff-space (ly:staff-symbol-staff-space grob))
         (line-thickness (ly:staff-symbol-line-thickness grob))
         (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot"))
         (dot-y-length (interval-length (ly:stencil-extent dot Y)))
         (stencil empty-stencil)
         ;; the two dots of the repeat sign should be centred at the
         ;; middle of the staff and neither should collide with staff
         ;; lines.
         ;; the required space is measured in line positions,
         ;; i.e. in half staff spaces.

         ;; dots are to fall into distict spaces, except when there's
         ;; only one space (and it's big enough to hold two dots and
         ;; some space between them)

         ;; choose defaults working without any staff
         (center 0.0)
         (dist (* 4 dot-y-length)))

    (if (> staff-space 0)
        (begin
          (set! dist (/ dist staff-space))
          (let ((staff-symbol (get-staff-symbol grob)))

            (if (ly:grob? staff-symbol)
                (let ((line-pos (staff-symbol-line-positions staff-symbol)))

                  (if (pair? line-pos)
                      (begin
                        (set! center
                              (interval-center (staff-symbol-line-span
                                                staff-symbol)))
                        ;; fold the staff into two at center
                        (let* ((folded-staff
                                (sort (map (lambda (lp) (abs (- lp center)))
                                           line-pos) <))
                               (gap-to-find (/ (+ dot-y-length line-thickness)
                                               (/ staff-space 2)))
                               (first (car folded-staff)))

                          ;; find the first space big enough
                          ;; to hold a dot and a staff line
                          ;; (a space in the folded staff may be
                          ;; narrower but can't be wider than the
                          ;; corresponding original spaces)
                          (set! dist
                                (or
                                 (any (lambda (x y)
                                        (and (> (- y x) gap-to-find)
                                             (+ x y)))
                                      folded-staff (cdr folded-staff))
                                 (if (< gap-to-find first)
                                     ;; there's a central space big
                                     ;; enough to hold both dots
                                     first

                                     ;; dots should go outside
                                     (+ (* 2 (last folded-staff))
                                        (/ (* 4 dot-y-length)
                                           staff-space))))))))))))
        (set! staff-space 1.0))

    (let* ((stencil empty-stencil)
           (stencil (ly:stencil-add stencil dot))
           (stencil (ly:stencil-translate-axis
                     stencil (* dist (/ staff-space 2)) Y))
           (stencil (ly:stencil-add stencil dot))
           (stencil (ly:stencil-translate-axis
                     stencil (* (- center (/ dist 2))
                                (/ staff-space 2)) Y)))
      stencil)))


(define (make-dotted-bar-line grob extent)
  "Draw a dotted bar line."
  (let* ((position (round (* (interval-end extent) 2)))
         (correction (if (even? position) 0.5 0.0))
         (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot"))
         (i (round (+ (interval-start extent)
                      (- 0.5 correction))))
         (e (round (+ (interval-end extent)
                      (- 0.5 correction))))
         (counting (interval-length (cons i e)))
         (stil-list (map
                     (lambda (x)
                       (ly:stencil-translate-axis
                        dot (+ x correction) Y))
                     (iota counting i 1))))

    (define (add-stencils! stil l)
      (if (null? l)
          stil
          (if (null? (cdr l))
              (ly:stencil-add stil (car l))
              (add-stencils! (ly:stencil-add stil (car l)) (cdr l)))))

    (add-stencils! empty-stencil stil-list)))

(define (make-dashed-bar-line grob extent)
  "Draw a dashed bar line."
  (let* ((height (interval-length extent))
         (staff-symbol (get-staff-symbol grob))
         (staff-space (ly:staff-symbol-staff-space grob))
         (line-thickness (layout-line-thickness grob))
         (thickness (* (ly:grob-property grob 'hair-thickness 1)
                       line-thickness))
         (dash-size (- 1.0 (ly:grob-property grob 'gap 0.3)))
         (line-count (staff-symbol-line-count staff-symbol)))

    (if (< (abs (+ line-thickness
                   (* (1- line-count) staff-space)
                   (- height)))
           0.1)
        (let ((blot (layout-blot-diameter grob))
              (half-space (/ staff-space 2.0))
              (half-thick (/ line-thickness 2.0))
              (stencil empty-stencil))

          (for-each (lambda (i)
                      (let ((top-y (min (* (+ i dash-size) half-space)
                                        (+ (* (1- line-count) half-space)
                                           half-thick)))
                            (bot-y (max (* (- i dash-size) half-space)
                                        (- 0 (* (1- line-count) half-space)
                                           half-thick))))

                        (set! stencil
                              (ly:stencil-add
                               stencil
                               (ly:round-filled-box (cons 0 thickness)
                                                    (cons bot-y top-y)
                                                    blot)))))
                    (iota line-count (1- line-count) (- 2)))
          stencil)
        (let* ((dashes (/ height staff-space))
               (total-dash-size (/ height dashes))
               (factor (/ (- dash-size thickness) staff-space))
               (stencil (ly:stencil-translate-axis
                         (ly:make-stencil (list 'dashed-line
                                                thickness
                                                (* factor total-dash-size)
                                                (* (- 1 factor) total-dash-size)
                                                0
                                                height
                                                (* factor total-dash-size 0.5))
                                          (cons (/ thickness -2) (/ thickness 2))
                                          (cons 0 height))
                         (interval-start extent)
                         Y)))

          (ly:stencil-translate-axis stencil (/ thickness 2) X)))))


(define ((make-segno-bar-line show-segno) grob extent)
  "Draw a segno bar line. If @var{show-segno} is set to @code{#t},
the segno sign is drawn over the double bar line; otherwise, it
draws the span bar variant, i.e. without the segno sign."
  (let* ((line-thickness (layout-line-thickness grob))
         (segno-kern (* (ly:grob-property grob 'segno-kern 1) line-thickness))
         (thin-stil (make-simple-bar-line grob extent))
         (double-line-stil (ly:stencil-combine-at-edge
                            thin-stil
                            X
                            LEFT
                            thin-stil
                            segno-kern))
         (segno (ly:font-get-glyph (ly:grob-default-font grob)
                                   "scripts.varsegno"))
         (stencil (ly:stencil-add
                   (if show-segno
                       segno
                       (ly:make-stencil
                        ""
                        (ly:stencil-extent segno X)
                        (cons 0 0)))
                   (ly:stencil-translate-axis
                    double-line-stil
                    (* 1/2 segno-kern)
                    X))))

    stencil))

(define (make-kievan-bar-line grob extent)
  "Draw a kievan bar line."
  (let* ((font (ly:grob-default-font grob))
         (stencil (stencil-whiteout-box
                   (ly:font-get-glyph font "scripts.barline.kievan"))))

    ;; the kievan bar line has no staff lines underneath,
    ;; so we whiteout-box them and move the grob to a higher layer
    (ly:grob-set-property! grob 'layer 1)
    stencil))

(define ((make-bracket-bar-line dir) grob extent)
  "Draw a bracket-style bar line. If @var{dir} is set to @code{LEFT}, the
opening bracket will be drawn, for @code{RIGHT} we get the closing bracket."
  (let* ((thick-stil (make-thick-bar-line grob extent))
         (brackettips-up (ly:font-get-glyph (ly:grob-default-font grob)
                                            "brackettips.up"))
         (brackettips-down (ly:font-get-glyph (ly:grob-default-font grob)
                                              "brackettips.down"))
         ;; the x-extent of the brackettips must not be taken into account
         ;; for bar line constructs like "[|:", so we set new bounds:
         (tip-up-stil (ly:make-stencil (ly:stencil-expr brackettips-up)
                                       (cons 0 0)
                                       (ly:stencil-extent brackettips-up Y)))
         (tip-down-stil (ly:make-stencil (ly:stencil-expr brackettips-down)
                                         (cons 0 0)
                                         (ly:stencil-extent brackettips-down Y)))
         (stencil (ly:stencil-add
                   thick-stil
                   (ly:stencil-translate-axis tip-up-stil
                                              (interval-end extent)
                                              Y)
                   (ly:stencil-translate-axis tip-down-stil
                                              (interval-start extent)
                                              Y))))

    (if (eqv? dir LEFT)
        stencil
        (flip-stencil X stencil))))

(define ((make-spacer-bar-line glyph) grob extent)
  "Draw an invisible bar line which has the same dimensions as the one
drawn by the procedure associated with glyph @var{glyph}."
  (let* ((stil (glyph->stencil glyph grob extent))
         (stil-x-extent (ly:stencil-extent stil X)))

    (ly:make-stencil "" stil-x-extent extent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar line callbacks

(define-public (ly:bar-line::calc-bar-extent grob)
  (let ((staff-symbol (get-staff-symbol grob))
        (staff-extent (cons 0 0)))

    (if (ly:grob? staff-symbol)
        (let ((bar-line-color (ly:grob-property grob 'color))
              (staff-color (ly:grob-property staff-symbol 'color))
              (half-staff-line-thickness (/ (ly:staff-symbol-line-thickness grob) 2))
              (staff-space (ly:staff-symbol-staff-space grob)))

          (set! staff-extent (ly:staff-symbol::height staff-symbol))

          (if (zero? staff-space)
              (set! staff-space 1.0))

          (if (< (interval-length staff-extent) staff-space)
              ;; staff is too small (perhaps consists of a single line);
              ;; extend the bar line to make it visible
              (set! staff-extent
                    (interval-widen staff-extent staff-space))
              ;; Due to rounding problems, bar lines extending to the outermost edges
              ;; of the staff lines appear wrongly in on-screen display
              ;; (and, to a lesser extent, in print) - they stick out a pixel.
              ;; The solution is to extend bar lines only to the middle
              ;; of the staff line - unless they have different colors,
              ;; when it would be undesirable.
              ;;
              ;; This reduction should not influence whether the bar is to be
              ;; expanded later, so length is not updated on purpose.
              (if (eq? bar-line-color staff-color)
                  (set! staff-extent
                        (interval-widen staff-extent
                                        (- half-staff-line-thickness)))))))
    staff-extent))

;; this function may come in handy when defining new bar line glyphs, so
;; we make it public.
;; This code should not be included in ly:bar-line::calc-bar-extent, because
;; this may confuse the drawing functions for dashed and dotted bar lines.
(define-public (bar-line::widen-bar-extent-on-span grob extent)
  "Widens the bar line @var{extent} towards span bars adjacent to grob @var{grob}."
  (let ((staff-symbol (get-staff-symbol grob))
        (has-span-bar (ly:grob-object grob 'has-span-bar)))

    (if (and (ly:grob? staff-symbol)
             (pair? has-span-bar))
        (let ((bar-line-color (ly:grob-property grob 'color))
              (staff-color (ly:grob-property staff-symbol 'color))
              (half-staff-line-thickness (/ (ly:staff-symbol-line-thickness grob) 2)))
          (if (eq? bar-line-color staff-color)
              ;; if the colors are equal, ly:bar-line::calc-bar-extent has
              ;; shortened the bar line extent by a half-staff-line-thickness
              ;; this is reverted on the interval bounds where span bars appear:
              (begin
                (and (ly:grob? (car has-span-bar))
                     (set! extent (cons (- (car extent) half-staff-line-thickness)
                                        (cdr extent))))
                (and (ly:grob? (cdr has-span-bar))
                     (set! extent (cons (car extent)
                                        (+ (cdr extent) half-staff-line-thickness))))))))
    extent))

(define (bar-line::bar-y-extent grob refpoint)
  "Compute the y-extent of the bar line relative to @var{refpoint}."
  (let* ((extent (ly:grob-property grob 'bar-extent '(0 . 0)))
         (rel-y (ly:grob-relative-coordinate grob refpoint Y))
         (y-extent (coord-translate extent rel-y)))

    y-extent))

(define-public (ly:bar-line::print grob)
  "The print routine for bar lines."
  (let ((glyph-name (ly:grob-property grob 'glyph-name))
        (extent (ly:grob-property grob 'bar-extent '(0 . 0))))

    (if (and glyph-name
             (> (interval-length extent) 0))
        (bar-line::compound-bar-line grob glyph-name extent)
        #f)))

(define-public (bar-line::compound-bar-line grob bar-glyph extent)
  "Build the bar line stencil."
  (let* ((line-thickness (layout-line-thickness grob))
         (kern (* (ly:grob-property grob 'kern 1) line-thickness))
         (bar-glyph-list (string->string-list
                          (strip-string-annotation bar-glyph)))
         (span-glyph (get-span-glyph bar-glyph))
         (span-glyph-list (string->string-list span-glyph))
         (neg-stencil empty-stencil)
         (stencil empty-stencil)
         (is-first-neg-stencil #t)
         (is-first-stencil #t))

    ;; We build up two separate stencils first:
    ;; (1) the neg-stencil is built from all glyphs that have
    ;;     a replacement-char in the span bar
    ;; (2) the main stencil is built from all remaining glyphs
    ;;
    ;; Afterwards the neg-stencil is attached left to the
    ;; stencil; this ensures that the main stencil starts
    ;; at x = 0.
    ;;
    ;; For both routines holds:
    ;; we stack the stencils obtained by the corresponding
    ;; single glyphs with spacing 'kern' except for the
    ;; first stencil
    ;; (Thanks to Harm who came up with this idea!)
    (for-each (lambda (bar span)
                (if (and (string=? span (string replacement-char))
                         is-first-stencil)
                    (begin
                      (set! neg-stencil
                            (ly:stencil-combine-at-edge
                             neg-stencil
                             X
                             RIGHT
                             (glyph->stencil bar grob extent)
                             (if is-first-neg-stencil 0 kern)))
                      (set! is-first-neg-stencil #f))
                    (begin
                      (set! stencil
                            (ly:stencil-combine-at-edge
                             stencil
                             X
                             RIGHT
                             (glyph->stencil bar grob extent)
                             (if is-first-stencil 0 kern)))
                      (set! is-first-stencil #f))))
              bar-glyph-list span-glyph-list)
    ;; if we have a non-empty neg-stencil,
    ;; we attach it to the left side of the stencil
    (and (not is-first-neg-stencil)
         (set! stencil
               (ly:stencil-combine-at-edge
                stencil
                X
                LEFT
                neg-stencil
                kern)))
    stencil))

(define-public (ly:bar-line::calc-anchor grob)
  "Calculate the anchor position of a bar line. The anchor is used for
the correct placement of bar numbers etc."
  (let* ((bar-glyph (ly:grob-property grob 'glyph-name ""))
         (bar-glyph-list (string->string-list (strip-string-annotation bar-glyph)))
         (span-glyph (assoc-get bar-glyph span-bar-glyph-alist bar-glyph))
         (x-extent (ly:grob-extent grob grob X))
         (anchor 0.0))

    (and (> (interval-length x-extent) 0)
         (if (or (= (length bar-glyph-list) 1)
                 ;; 'span-glyph' may be #f, thus use equal? and not string=? for
                 ;; comparing equality
                 (equal? bar-glyph span-glyph)
                 (equal? span-glyph ""))
             ;; We use the x-extent of the stencil if either
             ;; - we have a single bar-glyph
             ;; - bar-glyph and span-glyph are identical
             ;; - we have no span-glyph
             (set! anchor (interval-center x-extent))
             ;; If the conditions above do not hold,the anchor is the
             ;; center of the corresponding span bar stencil extent
             (set! anchor (interval-center
                           (ly:stencil-extent
                            (span-bar::compound-bar-line grob bar-glyph dummy-extent)
                            X)))))
    anchor))

(define-public (bar-line::calc-glyph-name grob)
  "Determine the @code{glyph-name} of the bar line depending on the
line break status."
  (let* ((glyph (ly:grob-property grob 'glyph))
         (dir (ly:item-break-dir grob))
         (result (assoc-get glyph bar-glyph-alist))
         (glyph-name (if (= dir CENTER)
                         glyph
                         (if (and result
                                  (string? (index-cell result dir)))
                             (index-cell result dir)
                             #f))))
    glyph-name))

(define-public (bar-line::calc-break-visibility grob)
  "Calculate the visibility of a bar line at line breaks."
  (let* ((glyph (ly:grob-property grob 'glyph))
         (result (assoc-get glyph bar-glyph-alist)))

    (if result
        (vector (string? (car result)) #t (string? (cdr result)))
        all-invisible)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; span bar callbacks

(define-public (ly:span-bar::calc-glyph-name grob)
  "Return the @code{'glyph-name} of the corresponding BarLine grob.
The corresponding SpanBar glyph is computed within
@code{span-bar::compound-bar-line}."
  (let* ((elts (ly:grob-object grob 'elements))
         (pos (1- (ly:grob-array-length elts)))
         (glyph-name '()))

    (while (and (eq? glyph-name '())
                (> pos -1))
           (begin (set! glyph-name
                        (ly:grob-property (ly:grob-array-ref elts pos)
                                          'glyph-name))
                  (set! pos (1- pos))))
    (if (eq? glyph-name '())
        (begin (ly:grob-suicide! grob)
               (set! glyph-name "")))
    glyph-name))

(define-public (ly:span-bar::width grob)
  "Compute the width of the SpanBar stencil."
  (let ((width (cons 0 0)))

    (if (grob::is-live? grob)
        (let* ((glyph-name (ly:grob-property grob 'glyph-name))
               (stencil (span-bar::compound-bar-line grob
                                                     glyph-name
                                                     dummy-extent)))

          (set! width (ly:stencil-extent stencil X))))
    width))

(define-public (ly:span-bar::before-line-breaking grob)
  "A dummy callback that kills the Grob @var{grob} if it contains
no elements."
  (let ((elts (ly:grob-object grob 'elements)))

    (if (zero? (ly:grob-array-length elts))
        (ly:grob-suicide! grob))))

(define-public (span-bar::compound-bar-line grob bar-glyph extent)
  "Build the stencil of the span bar."
  (let* ((line-thickness (layout-line-thickness grob))
         (kern (* (ly:grob-property grob 'kern 1) line-thickness))
         (bar-glyph-list (string->string-list
                          (strip-string-annotation bar-glyph)))
         (span-glyph (assoc-get bar-glyph span-bar-glyph-alist 'undefined))
         (stencil empty-stencil))

    (if (string? span-glyph)
        (let ((span-glyph-list (string->string-list span-glyph))
              (is-first-stencil #t))

          (for-each (lambda (bar span)
                      ;; the stencil stack routine is similar to the one
                      ;; used in bar-line::compound-bar-line, but here,
                      ;; leading replacement-chars are discarded.
                      (if (not (and (string=? span (string replacement-char))
                                    is-first-stencil))
                          (begin
                            (set! stencil
                                  (ly:stencil-combine-at-edge
                                   stencil
                                   X
                                   RIGHT
                                   ;; if the current glyph is the replacement-char,
                                   ;; we take the corresponding glyph from the
                                   ;; bar-glyph-list and insert an empty stencil
                                   ;; with the appropriate width.
                                   ;; (this method would fail if the bar-glyph-list
                                   ;; were shorter than the span-glyph-list,
                                   ;; but this makes hardly any sense from a
                                   ;; typographical point of view
                                   (if (string=? span (string replacement-char))
                                       ((make-spacer-bar-line bar) grob extent)
                                       (glyph->stencil span grob extent))
                                   (if is-first-stencil 0 kern)))
                            (set! is-first-stencil #f))))
                    bar-glyph-list span-glyph-list))
        ;; if span-glyph is not a string, it may be #f or 'undefined;
        ;; the latter signals that the span bar for the current bar-glyph
        ;; is undefined, so we raise a warning.
        (if (eq? span-glyph 'undefined)
            (ly:warning
             (_ "No span bar glyph defined for bar glyph '~a'; ignoring.")
             bar-glyph)))
    stencil))

;; The method used in the following routine depends on bar_engraver
;; not being removed from staff context.  If bar_engraver is removed,
;; the size of the staff lines is evaluated as 0, which results in a
;; solid span bar line with faulty y coordinate.
;;
;; This routine was originally by Juergen Reuter, but it was on the
;; bulky side. Rewritten by Han-Wen. Ported from c++ to Scheme by Marc Hohl.
(define-public (ly:span-bar::print grob)
  "The print routine for span bars."
  (let* ((elts-array (ly:grob-object grob 'elements))
         (refp (ly:grob-common-refpoint-of-array grob elts-array Y))
         (elts (reverse (sort (ly:grob-array->list elts-array)
                              ly:grob-vertical<?)))
         ;; Elements must be ordered according to their y coordinates
         ;; relative to their common axis group parent.
         ;; Otherwise, the computation goes mad.
         (bar-glyph (ly:grob-property grob 'glyph-name))
         (span-bar empty-stencil))

    (if (string? bar-glyph)
        (let ((extents '())
              (make-span-bars '())
              (model-bar #f))

          ;; we compute the extents of each system and store them
          ;; in a list; dito for the 'allow-span-bar property.
          ;; model-bar takes the bar grob, if given.
          (for-each (lambda (bar)
                      (let ((ext (bar-line::bar-y-extent bar refp))
                            (staff-symbol (ly:grob-object bar 'staff-symbol)))

                        (if (ly:grob? staff-symbol)
                            (let ((refp-extent (ly:grob-extent staff-symbol refp Y)))

                              (set! ext (interval-union ext refp-extent))

                              (if (> (interval-length ext) 0)
                                  (begin
                                    (set! extents (append extents (list ext)))
                                    (set! model-bar bar)
                                    (set! make-span-bars
                                          (append make-span-bars
                                                  (list (ly:grob-property
                                                         bar
                                                         'allow-span-bar
                                                         #t))))))))))
                    elts)
          ;; if there is no bar grob, we use the callback argument
          (if (not model-bar)
              (set! model-bar grob))
          ;; we discard the first entry in make-span-bars,
          ;; because its corresponding bar line is the
          ;; uppermost and therefore not connected to
          ;; another bar line
          (if (pair? make-span-bars)
              (set! make-span-bars (cdr make-span-bars)))
          ;; the span bar reaches from the lower end of the upper staff
          ;; to the upper end of the lower staff - when allow-span-bar is #t
          (reduce (lambda (curr prev)
                    (let ((span-extent (cons 0 0))
                          (allow-span-bar (car make-span-bars)))

                      (set! make-span-bars (cdr make-span-bars))
                      (if (> (interval-length prev) 0)
                          (begin
                            (set! span-extent (cons (cdr prev)
                                                    (car curr)))
                            ;; draw the span bar only when the staff lines
                            ;; don't overlap and allow-span-bar is #t:
                            (and (> (interval-length span-extent) 0)
                                 allow-span-bar
                                 (set! span-bar
                                       (ly:stencil-add
                                        span-bar
                                        (span-bar::compound-bar-line
                                         model-bar
                                         bar-glyph
                                         span-extent))))))
                      curr))
                  "" extents)
          (set! span-bar (ly:stencil-translate-axis
                          span-bar
                          (- (ly:grob-relative-coordinate grob refp Y))
                          Y))))
    span-bar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; volta bracket functions

(define-public (allow-volta-hook bar-glyph)
  "Allow the volta bracket hook being drawn over bar line @var{bar-glyph}."
  (if (string? bar-glyph)
      (set! volta-bracket-allow-volta-hook-list
            (append volta-bracket-allow-volta-hook-list
                    (list bar-glyph)))
      (ly:warning (_ ("Volta hook bar line must be a string; ignoring '~a'.")
                     bar-glyph))))

(define-session volta-bracket-allow-volta-hook-list '())

(define-public (volta-bracket::calc-hook-visibility bar-glyph)
  "Determine the visibility of the volta bracket end hook. It is
called in @code{lily/volta-bracket.cc} and returns @code{#t} if
@emph{no} hook should be drawn."
  (not (member bar-glyph volta-bracket-allow-volta-hook-list)))

(define-public (ly:volta-bracket::calc-shorten-pair grob)
  "Calculate the @code{shorten-pair} values for an ideal placement
of the volta brackets relative to the bar lines."
  (let* ((line-thickness (layout-line-thickness grob))
         (volta-half-line-thickness (* (ly:grob-property grob 'thickness 1.6)
                                       line-thickness
                                       1/2))
         (bar-array (ly:grob-object grob 'bars))
         ;; the bar-array starts with the uppermost bar line grob that is
         ;; covered by the left edge of the volta bracket; more (span)
         ;; bar line grobs from other staves may follow
         (left-bar-line (and (ly:grob-array? bar-array)
                             (positive? (ly:grob-array-length bar-array))
                             (ly:grob-array-ref bar-array 0)))
         ;; we need the vertical-axis-group-index of the left-bar-line
         ;; to find the corresponding right-bar-line
         (vag-index (and left-bar-line
                         (ly:grob-get-vertical-axis-group-index left-bar-line)))
         ;; the bar line corresponding to the right edge of the volta bracket
         ;; is the last entry with the same vag-index, so we transform the array to a list,
         ;; reverse it and search for the first suitable entry from
         ;; the back
         (right-bar-line (and left-bar-line
                              (find (lambda (e)
                                      (eqv? (ly:grob-get-vertical-axis-group-index e)
                                            vag-index))
                                    (reverse (ly:grob-array->list bar-array)))))
         ;; the left-bar-line may be a #'<Grob Item >,
         ;; so we add "" as a fallback return value
         (left-bar-glyph-name (if left-bar-line
                                  (ly:grob-property left-bar-line 'glyph-name "")
                                  (string annotation-char)))
         (right-bar-glyph-name (if right-bar-line
                                   (ly:grob-property right-bar-line 'glyph-name "")
                                   (string annotation-char)))
         ;; This is the original logic.  It flags left-bar-broken if
         ;; there is no left-bar-line.  That seems strange.
         (left-bar-broken (not (and left-bar-line
                                    (zero? (ly:item-break-dir left-bar-line)))))
         (right-bar-broken (not (and right-bar-line
                                     (zero? (ly:item-break-dir
                                             right-bar-line)))))
         ;; Revert to current grob for getting layout info if no
         ;; left-bar-line available
         (left-span-stencil-extent (ly:stencil-extent
                                    (span-bar::compound-bar-line
                                     (or left-bar-line grob)
                                     left-bar-glyph-name
                                     dummy-extent)
                                    X))
         (right-span-stencil-extent (ly:stencil-extent
                                     (span-bar::compound-bar-line
                                      (or right-bar-line grob)
                                      right-bar-glyph-name
                                      dummy-extent)
                                     X))
         (left-shorten 0.0)
         (right-shorten 0.0))

    ;; since "empty" intervals may look like (1.0 . -1.0), we use the
    ;; min/max functions to make sure that the placement is not corrupted
    ;; in case of empty bar lines
    (set! left-shorten
          (if left-bar-broken
              (- (max 0 (interval-end left-span-stencil-extent))
                 (max 0 (interval-end (ly:stencil-extent
                                       (bar-line::compound-bar-line
                                        (or left-bar-line grob)
                                        left-bar-glyph-name
                                        dummy-extent)
                                       X)))
                 volta-half-line-thickness)
              (- (max 0 (interval-end left-span-stencil-extent))
                 volta-half-line-thickness)))

    (set! right-shorten
          (if right-bar-broken
              (+ (- (max 0 (interval-end right-span-stencil-extent)))
                 volta-half-line-thickness)
              (- (min 0 (interval-start right-span-stencil-extent))
                 volta-half-line-thickness)))

    (cons left-shorten right-shorten)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predefined bar glyph print procedures

(add-bar-glyph-print-procedure "" make-empty-bar-line)
(add-bar-glyph-print-procedure "|" make-simple-bar-line)
(add-bar-glyph-print-procedure "." make-thick-bar-line)
(add-bar-glyph-print-procedure "!" make-dashed-bar-line)
(add-bar-glyph-print-procedure "'" make-tick-bar-line)
(add-bar-glyph-print-procedure ":" make-colon-bar-line)
(add-bar-glyph-print-procedure ";" make-dotted-bar-line)
(add-bar-glyph-print-procedure "k" make-kievan-bar-line)
(add-bar-glyph-print-procedure "S" (make-segno-bar-line #t))
(add-bar-glyph-print-procedure "=" (make-segno-bar-line #f))
(add-bar-glyph-print-procedure "[" (make-bracket-bar-line LEFT))
(add-bar-glyph-print-procedure "]" (make-bracket-bar-line RIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predefined bar lines
;;
;; definition of bar lines goes as follows:
;;
;; (define-bar-line "normal bar[-annotation]" "end of line" "start of line" "span bar")
;;
;; each entry has to be a string or #f.
;; The empty string "" is allowed and yields in an invisible bar line,
;; whereas #f reads 'no stencil'.
;;
;; Convention: if two bar lines would be identical in their
;; unbroken bar glyph, we use annotations to make them distinct;
;; as a general rule of thumb the main difference in their
;; behavior at the end of a line is used as annotation, cf.
;;
;; (define-bar-line ".|:" "|" ".|:" ".|")
;; (define-bar-line ".|:-||" "||" ".|:" ".|")
;;
;; or
;;
;; (define-bar-line "S-|" "|" "S" "=")
;; (define-bar-line "S-S" "S" "" "=")

;; common bar lines
(define-bar-line "" "" "" #f)
(define-bar-line "-" #f #f #f)
(define-bar-line "|" "|" #f "|")
(define-bar-line "|-s" #f "|" "|")
(define-bar-line "." "." #f ".")
(define-bar-line ".|" "|" ".|" ".|")
(define-bar-line "|." "|." #f "|.")
(define-bar-line "||" "||" #f "||")
(define-bar-line ".." ".." #f "..")
(define-bar-line "|.|" "|.|" #f "|.|")
(define-bar-line "!" "!" #f "!")
(define-bar-line ";" ";" #f ";")
(define-bar-line "'" "'" #f #f)

;; repeats
(define-bar-line ":|.:" ":|." ".|:"  " |.")
(define-bar-line ":..:" ":|." ".|:" " ..")
(define-bar-line ":|.|:" ":|." ".|:" " |.|")
(define-bar-line ":.|.:" ":|." ".|:" " .|.")
(define-bar-line ":|." ":|." #f " |.")
(define-bar-line ".|:" "|" ".|:" ".|")
(define-bar-line "[|:" "|" "[|:" " |")
(define-bar-line ":|]" ":|]" #f " | ")
(define-bar-line ":|][|:" ":|]" "[|:" " |  |")
(define-bar-line ".|:-||" "||" ".|:" ".|")

;; segno bar lines
(define-bar-line "S" "||" "S" "=")
(define-bar-line "S-|" "|" "S" "=")
(define-bar-line "S-S" "S" #f "=")
(define-bar-line ":|.S" ":|." "S" " |.")
(define-bar-line ":|.S-S" ":|.S" "" " |.")
(define-bar-line "S.|:" "|" "S.|:" " .|")
(define-bar-line "S.|:-S" "S" ".|:" " .|")
(define-bar-line ":|.S.|:" ":|." "S.|:" " |. .|")
(define-bar-line ":|.S.|:-S" ":|.S" ".|:" " |. .|")

;; ancient bar lines
(define-bar-line "k" "k" #f #f) ;; kievan style

;; volta hook settings
(allow-volta-hook ":|.")
(allow-volta-hook "|.")
(allow-volta-hook ":..:")
(allow-volta-hook ":|.|:")
(allow-volta-hook ":|.:")
(allow-volta-hook ":|.S")
(allow-volta-hook ":|.S-S")
(allow-volta-hook ":|.S.|:")
(allow-volta-hook ":|.S.|:-S")
(allow-volta-hook ":|]")
(allow-volta-hook ":|][|:")
