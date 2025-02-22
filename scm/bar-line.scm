;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2009--2023 Marc Hohl <marc@hohlart.de>
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions for staff and layout properties

(define (bar-line::calc-blot thickness extent grob)
  "Calculate the blot diameter for a rounded box, taking the extent
into account."
  (let ((blot-diameter (layout-blot-diameter grob))
        (height (interval-length extent)))

    (cond ((< thickness blot-diameter) thickness)
          ((< height blot-diameter) height)
          (else blot-diameter))))

(define-public (bar-line::draw-filled-box x-ext y-ext thickness extent grob)
  "Return a straight bar line created by @code{ly:round-@/filled-box}
looking at @var{x-ext}, @var{y-ext}, and @var{thickness}.  The blot is
calculated from @var{extent} and @var{grob}.  @var{y-ext} is not
necessarily equal to @var{extent}."
  (let* ((rounded (ly:grob-property grob 'rounded #f))
         (blot (if rounded (bar-line::calc-blot thickness extent grob) 0)))
    (ly:round-filled-box x-ext y-ext blot)))

(define (get-span-glyph bar-glyph)
  "Get the corresponding span glyph from the @code{span-glyph-bar-alist}.
Return the found span glyph unchanged if it is not of type @code{string?},
otherwise pad the string with @code{annotation-char}s to the length of the
@var{bar-glyph} string."
  (let ((span-glyph (assoc-get bar-glyph span-bar-glyph-alist bar-glyph)))

    (if (string? span-glyph)
        (string-pad-right
         span-glyph
         (string-length bar-glyph)
         replacement-char)
        span-glyph)))

(define (layout-blot-diameter grob)
  "Get the blot diameter of the @var{grob}'s corresponding layout."
  (let* ((layout (ly:grob-layout grob))
         (blot-diameter (ly:output-def-lookup layout 'blot-diameter)))

    blot-diameter))

(define (staff-symbol-line-count staff)
  "Get or compute the number of lines of staff @var{staff}."
  (if (ly:grob? staff)
      (let ((line-pos (ly:grob-property staff 'line-positions '())))
        (length line-pos))
      0))

(define (staff-symbol-line-span grob)
  (let ((line-positions (ly:grob-property grob 'line-positions)))
    (if (pair? line-positions)
        (fold
         (lambda (p iv)
           (add-point iv p))
         empty-interval
         line-positions)
        ;; See similar code in staff-symbol.cc.
        '(1.0 . -1.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal helper functions

(define annotation-char #\-)
(define replacement-char #\ )

(define dummy-extent (cons -1 1))


(define (glyph->stencil glyph is-span grob extent)
  "Return a stencil computed by the procedure associated with
glyph @var{glyph}. The arguments @var{grob} and @var{extent} are
mandatory to the procedures stored in @code{bar-glyph-print-procedures}."
  (let ((proc (assoc-get glyph bar-glyph-print-procedures))
        (stencil empty-stencil))

    (if (procedure? proc)
        (set! stencil (proc is-span grob extent))
        (ly:warning (G_ "Bar glyph ~a not known. Ignoring.") glyph))
    stencil))

(define-public (string->string-list str)
  "Convert string @var{str} into a list of strings with length@tie{}1.
@code{\"aBc\"} will be converted to @code{(\"a\" \"B\" \"c\")}.
For an empty string or if @var{str} is not of type @code{string?}, return a
list containing @code{\"\"}."
  (if (and (string? str)
           (not (string-null? str)))
      (map string (string->list str))
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
           (G_ "Annotation '~a' is allowed in the first argument of a bar line definition only.")
           str))))

(define (check-for-replacement str)
  "Check whether the replacement char is present in string @var{str}."
  (if (string? str)
      (if (string-index str replacement-char)
          (ly:warning
           (G_ "Replacement '~a' is allowed in the last argument of a bar line definition only.")
           str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line break decisions.

(define-public (define-bar-line bar-glyph eol-glyph bol-glyph span-glyph)
  "Define a bar glyph @var{bar-glyph} and its substitutes at the end of
a line (@var{eol-glyph}), at the beginning of a line (@var{bol-glyph})
and as a span bar (@var{span-glyph}).  The substitute glyphs may be
either strings or booleans: @code{#t} calls for the same value as
@var{bar-glyph} and @code{#f} calls for no glyph."

  ;; #t means copy the mid-line glyph
  (when (eq? eol-glyph #t)
    (set! eol-glyph bar-glyph))
  (when (eq? bol-glyph #t)
    (set! bol-glyph bar-glyph))
  (when (eq? span-glyph #t)
    (set! span-glyph bar-glyph))

  ;; The last argument should not include annotations.
  ;; But do not check this if identical to first argument,
  ;; (as when called with #t) to avoid a useless warning
  ;; while keeping the meaning of #t consistent in the interface.
  (when (not (equal? span-glyph bar-glyph))
    (check-for-annotation span-glyph))

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

(define-session-public bar-glyph-alist '())

(define-session-public span-bar-glyph-alist '())

(define-public (add-bar-glyph-print-procedure glyph proc)
  "Specify the single glyph @var{glyph} that calls print procedure @var{proc}.
The procedure @var{proc} has to be defined in the form
@code{(make-...-bar-line grob extent)} even if the @var{extent}
is not used within the routine."
  (if (or (not (string? glyph))
          (> (string-length glyph) 1))
      (ly:warning
       (G_ "add-bar-glyph-print-procedure: glyph '~a' has to be a single ASCII character.")
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

(define (make-no-bar-line is-span grob extent)
  "Return an empty stencil."
  empty-stencil)

(define (make-empty-bar-line is-span grob extent)
  "Draw an empty bar line."
  (ly:make-stencil "" (cons 0 0) extent))

(define (make-simple-bar-line is-span grob extent)
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

(define (make-thick-bar-line is-span grob extent)
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

(define (make-short-bar-line is-span grob extent)
  "Draw a short bar line."
  (let* ((line-thickness (layout-line-thickness grob))
         (thickness (* (ly:grob-property grob 'hair-thickness 1)
                       line-thickness))
         (short-extent (ly:grob-property grob 'short-bar-extent extent)))
    ;; Bypass bar-line::draw-filled-box to ignore the 'roundness property.
    ;; Full-height bar lines are squared off to meet the edge of the outer staff
    ;; lines, but that is not a concern for shorter bar lines.
    (ly:round-filled-box
     (cons 0 thickness) ; x
     short-extent ; y
     (bar-line::calc-blot thickness short-extent grob))))

(define (make-tick-bar-line is-span grob extent)
  "Draw a tick bar line."
  (let* ((line-thickness (layout-line-thickness grob))
         (thickness (* (ly:grob-property grob 'hair-thickness 1)
                       line-thickness))
         (staff-symbol (ly:grob-object grob 'staff-symbol))
         (line-pos (ly:grob-property staff-symbol 'line-positions))
         (line-count (length line-pos))
         (half-staff (* 1/2 (ly:staff-symbol-staff-space grob)))
         (center (interval-end extent)))

    ;; The provided extent is for normal bar lines, which do not
    ;; necessarily end at the top staff line: they are expected to be
    ;; extended if the staff is vertically short, and it is always
    ;; possible for BarLine.bar-extent to be overridden.  The tick is
    ;; expected to cross the top staff line, so we must find it.  If
    ;; there are fewer than two lines, we allow floating ticks so that
    ;; they are not confused with short bar lines.
    (if (>= line-count 2)
        (set! center (* (apply max line-pos) half-staff)))

    ;; Bypass bar-line::draw-filled-box to ignore the 'roundness property.
    ;; Full-height bar lines are squared off to meet the edge of the outer staff
    ;; lines, but that is not a concern for ticks.
    (ly:round-filled-box
     (cons 0 thickness) ; x
     (coord-translate (symmetric-interval half-staff) center) ; y
     (bar-line::calc-blot thickness extent grob))))

(define (make-colon-bar-line is-span grob extent)
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
         ;; i.e., in half staff spaces.

         ;; dots are to fall into distict spaces, except when there's
         ;; only one space (and it's big enough to hold two dots and
         ;; some space between them)

         ;; choose defaults working without any staff
         (center 0.0)
         (dist (* 4 dot-y-length)))

    (if (> staff-space 0)
        (begin
          (set! dist (/ dist staff-space))
          (let ((staff-symbol (ly:grob-object grob 'staff-symbol)))

            (if (ly:grob? staff-symbol)
                (let ((line-pos (ly:grob-property staff-symbol 'line-positions)))

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

;; A dotted line obviously contains a lined up amount of dots with space
;; inbetween, and probably before and behind.
;; Caused by different sized staves and user settings like \magnifyStaff,
;; layout-set-staff-size etc., the space between dots and the even the size of
;; the dots itself may differ, making a dotted bar line a beasty thing...
(define (make-dotted-bar-line is-span grob extent)
  "Draw a dotted bar line."
  (let* ((staff-space (ly:staff-symbol-staff-space grob))
         (top-pos (round (* (/ (interval-end extent) staff-space) 2)))
         (bottom-pos (round (* (/ (interval-start extent) staff-space) 2)))
         (dots-pos '())
         (dot (ly:font-get-glyph (ly:grob-default-font grob) "dots.dot")))

    (define (calc-dot-positions dot-position-interval)
      (if (not (interval-empty? dot-position-interval))
          (iota (1+ (/ (interval-length dot-position-interval) 2))
                (interval-start dot-position-interval)
                2)
          '()))

    (define (add! init ls rl)
      ;; Returns the list @code{rl} where the elements from @code{ls} are added to
      ;; the previous one, starting at @code{(+ init (car ls))}. Example:
      ;;   (add! 1.23 '(1 2 3) '())
      ;;   --> '(2.23 4.23 7.23)
      (if (null? ls)
          (reverse rl)
          (let ((new-init ((if (negative? init) - +) init (car ls))))
            (add! new-init (cdr ls) (cons new-init rl)))))

    ;; A staff group may contain staves with different staff space, like
    ;; TabStaff and Staff. If the SpanBar would use the staff space from above,
    ;; i.e. from the topmost staff, all other span bars would use these staff
    ;; space, even if not appropriate.
    ;; Instead we use the staff space from layout. Covers the majority of cases.
    (when is-span
      (let* ((height (interval-length extent))
             (grob-layout (ly:grob-layout grob))
             (layout-staff-space (ly:output-def-lookup grob-layout 'staff-space))
             (dot-count (round (/ height layout-staff-space)))
             (space (/ height dot-count))
             ;; Distribute dots evenly
             ;;
             ;; - For no dots we return an empty list.
             ;; - For one dot we want the same space from upper staff to dot
             ;; and from the dot to lower staff, i.e (/ space 2).
             ;; - For multiple dots we want said (/ space 2) from upper staff to
             ;; the topmost dot and from bottommost dot to lower staff.
             ;; Between the dots we go for `space'.
             ;;
             ;; Actually it is unneeded to add (/ space 2) at the end of `ls'
             ;; below (we also drop the result later), it makes understanding
             ;; easier, though.
             ;; -Harm
             (ls
              (if (zero? dot-count)
                  '()
                  (cons
                   (/ space 2)
                   (append
                    (make-list
                     (inexact->exact (1- dot-count))
                     space)
                    (list (/ space 2)))))))
        (set! dots-pos
              (if (pair? ls)
                  (map
                   (lambda (x) (* (/ x staff-space) 2))
                   (drop-right (add! (interval-end extent) ls '()) 1))
                  '()))))

    (when (not is-span)
      ;; Narrow the extent by half a dot on each end; if the
      ;; center of a dot is within the reduced interval, then the
      ;; whole dot is within the original interval.
      ;; If `layout-set-staff-size` is used, the thickness of staff lines change,
      ;; take it into account. `blot-diamter` as well
      (let* ((extent-less-dot
              (interval-widen
               extent
               (/ (interval-length (ly:stencil-extent dot Y)) -2)))
             (staff-line-thick (ly:staff-symbol-line-thickness grob))
             (bar-thick (ly:grob-property grob 'thick-thickness))
             (blot (bar-line::calc-blot bar-thick extent grob)))

        ;; If a dot is out of bounds due to rounding, bring it in.
        (when (> (+ top-pos staff-line-thick blot)
                 (/ (interval-end extent-less-dot) staff-space))
          (set! top-pos (1- top-pos)))
        (when (< (- bottom-pos staff-line-thick blot)
                 (/ (interval-start extent-less-dot) staff-space))
          (set! bottom-pos (1+ bottom-pos))))

      ;; The dots will be separated by one staff space center to
      ;; center, so they will be placed all in even or all in odd
      ;; positions.  Choose the alternative with more dots not
      ;; colliding with staff lines.
      (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
             (lines-pos (if (ly:grob? staff-symbol)
                            (ly:grob-property staff-symbol 'line-positions)
                            '()))
             (even-interval (cons
                             (if (even? bottom-pos)
                                 bottom-pos
                                 (1+ bottom-pos))
                             (if (even? top-pos)
                                 top-pos
                                 (1- top-pos))))
             (even-dots-pos (calc-dot-positions even-interval))
             (even-score (length (lset-difference
                                  = even-dots-pos lines-pos)))
             (odd-interval (cons
                            (if (odd? bottom-pos)
                                bottom-pos
                                (1+ bottom-pos))
                            (if (odd? top-pos)
                                top-pos
                                (1- top-pos))))
             (odd-dots-pos (calc-dot-positions odd-interval))
             (odd-score (length (lset-difference
                                 = odd-dots-pos lines-pos))))

        ;; choose the even or the odd configuration
        (if (= even-score odd-score)
            ;; break ties by choosing more dots
            (if (> (length odd-dots-pos) (length even-dots-pos))
                (set! dots-pos odd-dots-pos)
                (set! dots-pos even-dots-pos))
            ;; usually, take the config with more dots in spaces
            (if (> odd-score even-score)
                (set! dots-pos odd-dots-pos)
                (set! dots-pos even-dots-pos)))))

    (apply
     ly:stencil-add
     empty-stencil
     (map
      (lambda (pos)
        (ly:stencil-translate-axis dot (/ (* staff-space pos) 2) Y))
      dots-pos))))


(define (make-dashed-bar-line is-span grob extent)
  "Draw a dashed bar line."
  (let* ((height (interval-length extent))
         (staff-symbol (ly:grob-object grob 'staff-symbol grob))
         (staff-space (ly:staff-symbol-staff-space grob))
         (grob-layout (ly:grob-layout grob))
         (layout-staff-space (ly:output-def-lookup grob-layout 'staff-space))
         (half-space (/ (if is-span
                            layout-staff-space
                            staff-space)
                        2.0))
         (blot (layout-blot-diameter grob))
         (line-thickness (layout-line-thickness grob))
         (thickness (* (ly:grob-property grob 'hair-thickness 1)
                       line-thickness))
         (half-thick (/ line-thickness 2.0))
         (dash-size (- 1.0 (ly:grob-property grob 'gap 0.3)))
         (amount-of-dashes (/ height
                              (if is-span
                                  layout-staff-space
                                  staff-space)))
         (rounded-amount (round amount-of-dashes))
         (stencil empty-stencil))

    (for-each (lambda (i)
                (let ((top-y (min (* (+ i dash-size) half-space)
                                  (+ (* amount-of-dashes half-space)
                                     half-thick)))
                      (bot-y (max (* (- i dash-size) half-space)
                                  (- 0 (* amount-of-dashes half-space)
                                     half-thick))))
                  (set! stencil
                        (ly:stencil-add
                         stencil
                         (ly:round-filled-box (cons 0 thickness)
                                              (cons bot-y top-y)
                                              blot)))))
              (iota (1+ rounded-amount) rounded-amount (- 2)))
    (ly:stencil-translate-axis
     stencil
     (interval-center extent)
     Y)))


(define ((make-segno-bar-line show-segno) is-span grob extent)
  "Draw a segno bar line.  If @var{show-segno} is set to @code{#t},
the segno sign is drawn over the double bar line; otherwise, it
draws the span bar variant, i.e., without the segno sign."
  (let* ((line-thickness (layout-line-thickness grob))
         (segno-kern (* (ly:grob-property grob 'segno-kern 1) line-thickness))
         (thin-stil (make-simple-bar-line is-span grob extent))
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

(define (make-kievan-bar-line is-span grob extent)
  "Draw a kievan bar line."
  (let* ((font (ly:grob-default-font grob))
         (stencil (stencil-whiteout-box
                   (ly:font-get-glyph font "scripts.barline.kievan"))))

    ;; the kievan bar line has no staff lines underneath,
    ;; so we whiteout-box them and move the grob to a higher layer
    (ly:grob-set-property! grob 'layer 1)
    stencil))

(define ((make-bracket-bar-line dir) is-span grob extent)
  "Draw a bracket-style bar line. If @var{dir} is set to @code{LEFT}, the
opening bracket will be drawn, for @code{RIGHT} we get the closing bracket."
  (let* ((thick-stil (make-thick-bar-line is-span grob extent))
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

(define ((make-spacer-bar-line glyph) is-span grob extent)
  "Draw an invisible bar line which has the same dimensions as the one
drawn by the procedure associated with glyph @var{glyph}."
  (let* ((stil (glyph->stencil glyph is-span grob extent))
         (stil-x-extent (ly:stencil-extent stil X)))

    (ly:make-stencil "" stil-x-extent extent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar line callbacks

(define-public (ly:bar-line::calc-bar-extent grob)
  (let ((staff-symbol (ly:grob-object grob 'staff-symbol)))
    (if (ly:grob? staff-symbol)
        (let ((staff-extent (ly:grob-property staff-symbol 'widened-extent))
              (bar-line-color (ly:grob-property grob 'color))
              (staff-color (ly:grob-property staff-symbol 'color)))
          ;; FIXME: "red" not eq? to #(rgb-color 1 0 0)
          (if (eq? bar-line-color staff-color)
              ;; Due to rounding problems, bar lines extending to the outermost edges
              ;; of the staff lines appear wrongly in on-screen display
              ;; (and, to a lesser extent, in print) - they stick out a pixel.
              ;; The solution is to extend bar lines only to the middle
              ;; of the staff line - unless they have different colors,
              ;; when it would be undesirable.
              ;;
              ;; This reduction should not influence whether the bar is to be
              ;; expanded later, so length is not updated on purpose.
              (let ((half-staff-line-thickness
                     (* 1/2 (ly:staff-symbol-line-thickness grob))))
                (interval-widen staff-extent (- half-staff-line-thickness)))
              staff-extent))
        '(0 . 0))))

(define-public (ly:bar-line::calc-short-bar-extent grob)
  (let ((staff-symbol (ly:grob-object grob 'staff-symbol)))
    (if (ly:grob? staff-symbol)
        (let* ((staff-extent (ly:grob-property staff-symbol 'widened-extent))
               (normal-height (interval-length staff-extent))
               (staff-space (ly:staff-symbol-staff-space grob))
               (normal-height-spaces (/ normal-height staff-space))
               ;; Use half the height of the staff, rounded up to an integer
               ;; number of staff spaces.
               (short-height-spaces (truncate (/ (1+ normal-height-spaces) 2)))
               (center (interval-center staff-extent))
               (line-thickness (layout-line-thickness grob))
               (thickness (* (ly:grob-property grob 'hair-thickness 1)
                             line-thickness)))
          ;; If the normal bar lines are quite short, short bar lines will be
          ;; hard to distinguish or hard to see.  Render them like anti-ticks.
          (if (< normal-height-spaces 2)
              (begin
                (set! short-height-spaces 1)
                (set! center (interval-start staff-extent))))
          (coord-translate
           (symmetric-interval (/ (* short-height-spaces staff-space) 2))
           center))
        '(0 . 0))))

;; this function may come in handy when defining new bar line glyphs, so
;; we make it public.
;; This code should not be included in ly:bar-line::calc-bar-extent, because
;; this may confuse the drawing functions for dashed and dotted bar lines.
(define-public (bar-line::widen-bar-extent-on-span grob extent)
  "Widen the bar line @var{extent} towards span bars adjacent to grob @var{grob}."
  (let ((staff-symbol (ly:grob-object grob 'staff-symbol))
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
         (span-glyph-list
          (cond ((string? span-glyph)
                 (string->string-list span-glyph))
                ((boolean? span-glyph)
                 (make-list (length bar-glyph-list) span-glyph))
                (else
                 (ly:warning
                  "Setting for span bar needs to be a boolean or string: ~a. Using #f instead."
                  span-glyph)
                 (make-list (length bar-glyph-list) #f))))
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
                (if (and span
                         (string=? span (string replacement-char))
                         is-first-stencil)
                    (begin
                      (set! neg-stencil
                            (ly:stencil-combine-at-edge
                             neg-stencil
                             X
                             RIGHT
                             (glyph->stencil bar #f grob extent)
                             (if is-first-neg-stencil 0 kern)))
                      (set! is-first-neg-stencil #f))
                    (begin
                      (set! stencil
                            (ly:stencil-combine-at-edge
                             stencil
                             X
                             RIGHT
                             (glyph->stencil bar #f grob extent)
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

    (if (and (ly:grob-property grob 'right-justified #f)
             (< (ly:item-break-dir grob) 1))
        (ly:stencil-translate-axis
         stencil
         (- (cdr (ly:stencil-extent stencil X)))
         X)
        stencil)))

(define-public (ly:bar-line::calc-anchor grob)
  "Calculate the anchor position of a bar line. The anchor is used for
the correct placement of bar numbers, etc."
  (let* ((bar-glyph (ly:grob-property grob 'glyph-name ""))
         (bar-glyph-list
          (string->string-list (strip-string-annotation bar-glyph)))
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
             (set! anchor
                   ;; if a mid-line bar-line is right-justified, the anchor
                   ;; needs to be adjusted
                   (if (and (ly:grob-property grob 'right-justified #f)
                            (= (ly:item-break-dir grob) 0))
                       (- (interval-end x-extent))
                       (interval-center x-extent)))
             ;; If the conditions above do not hold,the anchor is the
             ;; center of the corresponding span bar stencil extent
             (let* ((span-bar-stencil
                     (span-bar::compound-bar-line grob bar-glyph dummy-extent))
                    (span-stil-x-extent (ly:stencil-extent span-bar-stencil X)))
               (set! anchor
                     ;; if a mid-line bar-line is right-justified, the anchor
                     ;; needs to be adjusted
                     (if (and (ly:grob-property grob 'right-justified #f)
                              (= (ly:item-break-dir grob) 0))
                         (interval-end span-stil-x-extent)
                         (interval-center span-stil-x-extent))))))

    anchor))

(define-public (bar-line::calc-glyph-name-for-direction glyphs dir)
  "Find the glyph name for a bar line.  @code{glyphs} is the list of
bar-line types to consider in order.  Each must have been defined with
@code{define-bar-line}.  @var{dir} is the break direction to consider:
@code{LEFT} = end of line, @code{CENTER} = middle of line,
@code{RIGHT} = start of line."
  (define (get-glyph-name glyph)
    "Given a particular @code{glyph} value, get the entry for the relevant
direction from the alist."
    (let ((alist-entry (assoc-get glyph bar-glyph-alist)))
      (and alist-entry
           (begin
             (when (!= dir CENTER)
               (set! glyph (index-cell alist-entry dir)))
             ;; We want to ignore glyphs that do not yield a stencil.
             ;; Checking the result of glyph->stencil would probably
             ;; be ideal, but the difficulty is that glyph->stencil
             ;; requires a grob, which does not exist yet.  Instead,
             ;; we look for glyph "x" as a special case.
             (and (string? glyph)
                  (not (equal? "x" (strip-string-annotation glyph)))
                  glyph)))))

  (define (find-first-glyph-name glyphs)
    (and (pair? glyphs)
         (string? (car glyphs)) ; stop at #f for \noBar
         (or (get-glyph-name (car glyphs))
             (find-first-glyph-name (cdr glyphs)))))

  ;; If we return #f regardless of direction, Bar_engraver does not
  ;; create a BarLine.  If we don't have at least one string in the
  ;; glyph list, we don't want to create a BarLine.
  (and (pair? glyphs)
       (string? (car glyphs)) ; stop at #f for \noBar
       (or (find-first-glyph-name glyphs)
           ;; Bar line definitions can have "x" and #f in them to
           ;; allow lower layers to show through, but weird alignment
           ;; and spacing issues can occur when we create BarLine
           ;; grobs with no mid-line or end-of-line stencils, so we
           ;; default to "" in these cases.  We could investigate
           ;; whether we can eliminate these special cases.  (It isn't
           ;; clear that there are better alternatives.)
           (if (= dir RIGHT)
               #f
               ""))))

(define-public (bar-line::calc-glyph-name grob)
  "Return the name of the bar line glyph printed by @var{grob} for the
actual break direction."
  (let ((dir (ly:item-break-dir grob)))
    (cond
     ((= dir LEFT) (ly:grob-property grob 'glyph-left))
     ((= dir RIGHT) (ly:grob-property grob 'glyph-right))
     (else (ly:grob-property grob 'glyph)))))

(define-public (bar-line::calc-break-visibility grob)
  "Calculate the visibility of a bar line at line breaks."
  (vector
   (string? (ly:grob-property grob 'glyph-left))
   (string? (ly:grob-property grob 'glyph))
   (string? (ly:grob-property grob 'glyph-right))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; span bar callbacks

(define-public (ly:span-bar::calc-anchor grob)
  "Calculate the anchor position of the @code{SpanBar}. The anchor is
used for the correct placement of bar numbers, etc."
  (interval-center (ly:span-bar::width grob)))

(define-public (ly:span-bar::calc-glyph-name grob)
  "Return the @code{'glyph-name} of the corresponding @code{BarLine} grob.
The corresponding @code{SpanBar} glyph is computed within
@code{span-bar::@/compound-bar-line}."
  (let ((model-bar (ly:span-bar::choose-model-bar-line grob)))
    (if (grob::is-live? grob)
        (ly:grob-property model-bar 'glyph-name)
        ;; avoid recursion: model-bar is a dead span bar
        '())))

(define (ly:span-bar::choose-model-bar-line grob)
  "Choose one of the bar lines to use as a template for the span bar.
Ideally, this chooses the same bar line whether it is called from
ly:span-bar::width, ly:span-bar::print, etc.

N.B. If no model bar line is found, this function kills the span bar
and returns the dead span bar."

  (define (bar-acceptable? bar)
    "return bar if it is acceptable, otherwise return #f"
    (let ((staff-symbol (ly:grob-object bar 'staff-symbol #f)))
      (and staff-symbol
           (let* ((refp (ly:grob-common-refpoint bar staff-symbol Y))
                  (ext (interval-union
                        (bar-line::bar-y-extent bar refp)
                        (ly:grob-extent staff-symbol refp Y))))
             (and (positive? (interval-length ext)) bar)))))

  (let ((model-bar
         (find bar-acceptable?
               (ly:grob-array->list (ly:grob-object grob 'elements)))))
    (if model-bar
        model-bar
        (begin
          (ly:grob-suicide! grob)
          grob))))

(define-public (ly:span-bar::width grob)
  "Compute the width of the @code{SpanBar} stencil."
  ;; We don't use the grob's actual stencil because it would trigger
  ;; Y-axis calculations.
  (let ((model-bar (ly:span-bar::choose-model-bar-line grob))
        ;; note: choose-model-bar-line might have killed the span bar
        (glyph-name (ly:grob-property grob 'glyph-name)))
    (if (string? glyph-name)
        (let ((stencil (span-bar::compound-bar-line
                        model-bar
                        glyph-name
                        dummy-extent)))
          (ly:stencil-extent stencil X))
        empty-interval)))

(define-public (ly:span-bar::before-line-breaking grob)
  "A dummy callback that kills the @code{Grob} @var{grob} if it contains
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
                                       ((make-spacer-bar-line bar) #t grob extent)
                                       (glyph->stencil span #t grob extent))
                                   (if is-first-stencil 0 kern)))
                            (set! is-first-stencil #f))))
                    bar-glyph-list span-glyph-list))
        ;; if span-glyph is not a string, it may be #f or 'undefined;
        ;; the latter signals that the span bar for the current bar-glyph
        ;; is undefined, so we raise a warning.
        (if (eq? span-glyph 'undefined)
            (ly:warning
             (G_ "No span bar glyph defined for bar glyph '~a'; ignoring.")
             bar-glyph)))

    (if (and (ly:grob-property grob 'right-justified #f)
             (< (ly:item-break-dir grob) 1))
        (ly:stencil-translate-axis
         stencil
         (- (cdr (ly:stencil-extent stencil X)))
         X)
        stencil)))

;; The method used in the following routine depends on bar_engraver
;; not being removed from staff context.  If bar_engraver is removed,
;; the size of the staff lines is evaluated as 0, which results in a
;; solid span bar line with faulty y coordinate.
;;
;; This routine was originally by Juergen Reuter, but it was on the
;; bulky side. Rewritten by Han-Wen. Ported from c++ to Scheme by Marc Hohl.
(define-public (ly:span-bar::print grob)
  "The print routine for span bars."
  (let ((span-bar empty-stencil)
        (model-bar (ly:span-bar::choose-model-bar-line grob))
        ;; note: choose-model-bar-line might have killed the span bar
        (bar-glyph (ly:grob-property grob 'glyph-name)))

    (when (string? bar-glyph)
      (let* ((elts-array (ly:grob-object grob 'elements))
             (refp (ly:grob-common-refpoint-of-array grob elts-array Y))
             (elts (reverse (sort (ly:grob-array->list elts-array)
                                  ly:grob-vertical<?)))
             ;; Elements must be ordered according to their y coordinates
             ;; relative to their common axis group parent.
             ;; Otherwise, the computation goes mad.
             (extents '())
             (make-span-bars '()))

        ;; we compute the extents of each system and store them
        ;; in a list; dito for the 'allow-span-bar property.
        (for-each
         (lambda (bar)
           (let ((staff-symbol (ly:grob-object bar 'staff-symbol #f)))
             (when staff-symbol
               (let ((ext (interval-union
                           (bar-line::bar-y-extent bar refp)
                           (ly:grob-extent staff-symbol refp Y))))
                 (when (positive? (interval-length ext))
                   (set! extents (append extents (list ext)))
                   (set! make-span-bars
                         (append make-span-bars
                                 (list (ly:grob-property
                                        bar
                                        'allow-span-bar
                                        #t)))))))))
         elts)
        ;; we discard the first entry in make-span-bars,
        ;; because its corresponding bar line is the
        ;; uppermost and therefore not connected to
        ;; another bar line
        (when (pair? make-span-bars)
          (set! make-span-bars (cdr make-span-bars)))
        ;; the span bar reaches from the lower end of the upper staff
        ;; to the upper end of the lower staff - when allow-span-bar is #t
        (reduce (lambda (curr prev)
                  (let ((span-extent (cons 0 0))
                        (allow-span-bar (car make-span-bars)))

                    (set! make-span-bars (cdr make-span-bars))
                    (when (positive? (interval-length prev))
                      (set! span-extent (cons (cdr prev) (car curr)))
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
                                   span-extent)))))
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
      (ly:warning (G_ ("Volta hook bar line must be a string; ignoring '~a'.")
                      bar-glyph))))

(define-session volta-bracket-allow-volta-hook-list '())

(define-public (volta-bracket::calc-hook-visibility bar-glyph)
  "Determine the visibility of the volta bracket end hook, returning
@code{#t} if @emph{no} hook should be drawn."
  (not (member bar-glyph volta-bracket-allow-volta-hook-list)))

(define-public (ly:volta-bracket::calc-shorten-pair grob)
  "Calculate the @code{shorten-pair} values for an ideal placement
of the volta brackets relative to the bar lines."
  (let* ((line-thickness (layout-line-thickness grob))
         (volta-half-line-thickness (* (ly:grob-property grob 'thickness 1.6)
                                       line-thickness
                                       1/2))
         (left-bar-array (ly:grob-object grob 'bars-left))
         (left-bar-array-length (and (ly:grob-array? left-bar-array)
                                     (ly:grob-array-length left-bar-array)))
         (right-bar-array (ly:grob-object grob 'bars-right))
         (right-bar-array-length (and (ly:grob-array? right-bar-array)
                                      (ly:grob-array-length right-bar-array)))
         ;; left-bar-array starts with the uppermost bar line grob that is
         ;; covered by the left edge of the volta bracket; more (span)
         ;; bar line grobs from other staves may follow
         (left-bar-line (and (positive-number? left-bar-array-length)
                             (ly:grob-array-ref left-bar-array 0)))
         ;; we need the vertical-axis-group-index of the left-bar-line
         ;; to find the corresponding right-bar-line
         (vag-index (and left-bar-line
                         (ly:grob-get-vertical-axis-group-index left-bar-line)))
         (right-bar-line
          (if (positive-number? right-bar-array-length)
              (if left-bar-line
                  ;; the bar line corresponding to the right edge of the volta
                  ;; bracket is the last entry with the same vag-index, so we
                  ;; transform right-bar-array to a list, reverse it and search
                  ;; for the first suitable entry from the back
                  (find (lambda (e)
                          (eqv? (ly:grob-get-vertical-axis-group-index e)
                                vag-index))
                        (reverse (ly:grob-array->list right-bar-array)))
                  ;; if there is no bar line at the left edge, take the last
                  ;; entry from right-bar-array
                  (ly:grob-array-ref right-bar-array
                                     (1- right-bar-array-length)))
              #f))
         ;; the left-bar-line may be a #'<Grob Item >,
         ;; so we add "" as a fallback return value
         (left-bar-glyph-name (if left-bar-line
                                  (ly:grob-property left-bar-line
                                                    'glyph-name "")
                                  ""))
         (right-bar-glyph-name (if right-bar-line
                                   (ly:grob-property right-bar-line
                                                     'glyph-name "")
                                   ""))
         (no-left-bar-or-broken (not (and left-bar-line
                                          (zero? (ly:item-break-dir
                                                  left-bar-line)))))
         (no-right-bar-or-broken (not (and right-bar-line
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
         (right-shorten 0.0)
         ;; for volta brackets not starting at a bar line, we need to find out
         ;; whether the left bound contains prefatory stuff, or whether it is an
         ;; empty column
         (refp (ly:grob-system grob))
         (left-bound (ly:spanner-bound grob LEFT))
         (left-bound-extent (ly:grob-extent left-bound refp X)))
    ;; since "empty" intervals may look like (1.0 . -1.0), we use the
    ;; min/max functions to make sure that the placement is not corrupted
    ;; in case of empty bar lines
    (set! left-shorten
          (if no-left-bar-or-broken
              (- (max 0 (interval-end left-span-stencil-extent))
                 (max 0 (interval-end (ly:stencil-extent
                                       (bar-line::compound-bar-line
                                        (or left-bar-line grob)
                                        left-bar-glyph-name
                                        dummy-extent)
                                       X)))
                 volta-half-line-thickness
                 (if (interval-empty? left-bound-extent)
                     ;; value 0.5 is 'magic'; LilyPond sets this as the ideal
                     ;; spacing for empty columns, which we have to compensate
                     ;; for (see function
                     ;; `Spacing_spanner::standard_breakable_column_spacing`)
                     -0.5
                     ;; if the volta bracket starts right after the prefatory
                     ;; matter, shorten more by some ad-hoc value
                     (if left-bar-line 0 -1)))
              (- (max 0 (interval-end left-span-stencil-extent))
                 volta-half-line-thickness)))

    (set! right-shorten
          (if no-right-bar-or-broken
              (+ (- (max 0 (interval-end right-span-stencil-extent)))
                 volta-half-line-thickness)
              (- (min 0 (interval-start right-span-stencil-extent))
                 volta-half-line-thickness)))

    (cons left-shorten right-shorten)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predefined bar glyph print procedures

(add-bar-glyph-print-procedure "x" make-no-bar-line)
(add-bar-glyph-print-procedure "" make-empty-bar-line)
(add-bar-glyph-print-procedure "|" make-simple-bar-line)
(add-bar-glyph-print-procedure "." make-thick-bar-line)
(add-bar-glyph-print-procedure "!" make-dashed-bar-line)
(add-bar-glyph-print-procedure "'" make-tick-bar-line)
(add-bar-glyph-print-procedure "," make-short-bar-line)
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
;; (define-bar-line "mid-line bar[-annotation]"
;;                  "end-of-line bar[-annotation]"
;;                  "beginning-of-line bar[-annotation]"
;;                  "span bar")
;;
;; Each argument must be a string or #f.  The string "" calls for a
;; zero-width stencil.  The string "x" or the value #f call for no
;; stencil.  "x" may be annotated, unlike #f.
;;
;; Convention: if two bar lines would be identical in their
;; unbroken bar glyph, we use annotations to make them distinct;
;; as a general rule of thumb the main difference in their
;; behavior at the end of a line is used as annotation, e.g.,
;;
;;   (define-bar-line "S" #f #t "=")
;;   (define-bar-line "S-|" "|" #t "=")
;;   (define-bar-line "S-||" "||" #t "=")
;;
;; When adding a new bar type, you must also add it in
;; Documentation/en/included/bar-lines.ly.

;; common bar lines
(define-bar-line "" #t #f #f)
(define-bar-line "|" #t #f #t)
(define-bar-line "|-s" #f #t "|")
(define-bar-line "." #t #f #t)
(define-bar-line ".|" #f #t #t)
(define-bar-line ".|-|" "|" #t ".|")
(define-bar-line ".|-||" "||" #t ".|")
(define-bar-line "|." #t #f #t)
(define-bar-line "||" #t #f #t)
(define-bar-line ".." #t #f #t)
;; Should "|.|" break into "|." + ".|" as if it were a dotless double repeat?
;; Dan Eble contacted one arranger who has used it: Jeff Olson.
;;     https://www.mutopiaproject.org/ftp/MozartWA/KV265/guitar-duo-complete/
;;         guitar-duo-complete-let.pdf
;; Mozart's work has double repeat bars.  Olson wanted to skip the repetition
;; but show where it had been.  Splitting the bar line would suggest finality in
;; inappropriate places.
(define-bar-line "|.|" #t #f #t)
(define-bar-line "!" #t #f #t)
(define-bar-line ";" #t #f #t)
(define-bar-line "'" #t #f #f)
(define-bar-line "," #t #f #f)

;; end-of-line caesura
(define-bar-line "x-|" "|" #f #f)
(define-bar-line "x-||" "||" #f #f)
(define-bar-line "x-." "." #f #f)

;; repeats
(define-bar-line ":|.:" ":|." ".|:"  " |.")
(define-bar-line ":..:" ":|." ".|:" " ..")
(define-bar-line ":|.|:" ":|." ".|:" " |.|")
(define-bar-line ":.|.:" ":|." ".|:" " .|.")
(define-bar-line ":|." #t #f " |.")
(define-bar-line ".|:" #f #t ".|")
(define-bar-line ".|:-|" "|" #t ".|")
(define-bar-line ".|:-||" "||" #t ".|")
(define-bar-line ".|:-|." "|." #t ".|")
(define-bar-line "[|:" #f #t " |")
(define-bar-line "[|:-|" "|" #t " |")
(define-bar-line "[|:-||" "||" #t " |")
(define-bar-line "[|:-|." "|." #t " |")
(define-bar-line ":|]" #t #f " | ")
(define-bar-line ":|][|:" ":|]" "[|:" " |  |")

;; segno bar lines
(define-bar-line "S" #f #t "=")
(define-bar-line "S-|" "|" #t "=")
(define-bar-line "S-||" "||" #t "=")
(define-bar-line "S-S" #t #f "=")
(define-bar-line "|.S" "|." "S" "|.")
(define-bar-line "|.S-S" "|.S" #f "|.")
(define-bar-line ":|.S" ":|." "S" " |.")
(define-bar-line ":|.S-S" ":|.S" #f " |.")
(define-bar-line "S.|:" #f #t " .|")
(define-bar-line "S.|:-|" "|" #t " .|")
(define-bar-line "S.|:-||" "||" #t " .|")
(define-bar-line "S.|:-S" "S" ".|:" " .|")
(define-bar-line "|.S.|:" "|." "S.|:" "|. .|")
(define-bar-line "|.S.|:-S" "|.S" ".|:" "|. .|")
(define-bar-line ":|.S.|:" ":|." "S.|:" " |. .|")
(define-bar-line ":|.S.|:-S" ":|.S" ".|:" " |. .|")

;; ancient bar lines
(define-bar-line "k" #t #f #f) ;; kievan section/final bar line
(define-bar-line "-span|" #t #f "|") ; mensurstrich

;; The right side of a volta bracket is closed when the corresponding
;; end-of-line bar line glyph would be one of the following.  Whether
;; it is actually at a line break does not matter for this purpose.
;;
;; These signify either the end of a repeated section or the end of
;; the piece.  Built-in end-of-line bar glyphs with either meaning
;; should be listed here even if they are not normally used with volta
;; brackets.
;;
;; This complication is disappointing because this information is
;; readily available when it comes from commands such as \repeat;
;; however, we want to be able to derive meaning from manual \bar
;; commands also.
;;
;; end of repeated section
(allow-volta-hook ":|.")
(allow-volta-hook ":|.S")
(allow-volta-hook ":|]")
;; end of piece
(allow-volta-hook "|.")
(allow-volta-hook "|.S")
