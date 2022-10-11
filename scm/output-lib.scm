;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(use-modules (ice-9 match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general

(define-public (grob::has-interface grob iface)
  (memq iface (ly:grob-interfaces grob)))

(define-public (grob::is-live? grob)
  (pair? (ly:grob-basic-properties grob)))

(define-public (grob::name grob)
  "Return the name of the grob @var{grob} as a symbol."
  (assq-ref (ly:grob-property grob 'meta) 'name))

(define-public (grob::rhythmic-location grob)
  "Return a pair consisting of the measure number and moment within
   the measure of grob @var{grob}."
  (let* ((item (if (ly:spanner? grob)
                   (ly:spanner-bound grob LEFT)
                   grob))
         (col (ly:item-get-column item)))
    (if (ly:grob? col)
        (ly:grob-property col 'rhythmic-location)
        '())))

(define-public (grob::when grob)
  "Return the global timestep (a @code{Moment}) of grob @var{grob}."
  (let* ((item (if (ly:spanner? grob)
                   (ly:spanner-bound grob LEFT)
                   grob))
         (col (ly:item-get-column item)))
    (if (ly:grob? col)
        (ly:grob-property col 'when)
        '())))

(define-public (make-stencil-boxer thickness padding callback)
  "Return function that adds a box around the grob passed as argument."
  (lambda (grob)
    (box-stencil (callback grob) thickness padding)))

(define-public (make-stencil-circler thickness padding callback)
  "Return function that adds a circle around the grob passed as argument."
  (lambda (grob)
    (circle-stencil (callback grob) thickness padding)))

(define-public (print-circled-text-callback grob)
  (grob-interpret-markup grob (make-circle-markup
                               (ly:grob-property grob 'text))))

(define-public (event-cause grob)
  (let ((cause (ly:grob-property  grob 'cause)))

    (cond
     ((ly:stream-event? cause) cause)
     ((ly:grob? cause) (event-cause cause))
     (else #f))))

(define-public (grob-interpret-markup grob text)
  (let* ((layout (ly:grob-layout grob))
         (defs (ly:output-def-lookup layout 'text-font-defaults))
         (props (ly:grob-alist-chain grob defs)))

    (ly:text-interface::interpret-markup layout props text)))

(define-public (grob::unpure-Y-extent-from-stencil pure-function)
  "The unpure height will come from a stencil whereas the pure
   height will come from @code{pure-function}."
  (ly:make-unpure-pure-container ly:grob::stencil-height pure-function))

(define-public grob::unpure-horizontal-skylines-from-stencil
  (ly:make-unpure-pure-container
   ly:grob::horizontal-skylines-from-stencil
   ly:grob::pure-simple-horizontal-skylines-from-extents))

(define-public grob::always-horizontal-skylines-from-stencil
  (ly:make-unpure-pure-container
   ly:grob::horizontal-skylines-from-stencil))

(define-public grob::unpure-vertical-skylines-from-stencil
  (ly:make-unpure-pure-container
   ly:grob::vertical-skylines-from-stencil
   ly:grob::pure-simple-vertical-skylines-from-extents))

(define-public grob::always-vertical-skylines-from-stencil
  (ly:make-unpure-pure-container
   ly:grob::vertical-skylines-from-stencil))

(define-public grob::always-vertical-skylines-from-element-stencils
  (ly:make-unpure-pure-container
   ly:grob::vertical-skylines-from-element-stencils
   ly:grob::pure-vertical-skylines-from-element-stencils))

(define-public grob::always-horizontal-skylines-from-element-stencils
  (ly:make-unpure-pure-container
   ly:grob::horizontal-skylines-from-element-stencils
   ly:grob::pure-horizontal-skylines-from-element-stencils))

;; Using this as a callback for a grob's Y-extent promises
;; that the grob's stencil does not depend on line-spacing.
;; We use this promise to figure the space required by Clefs
;; and such at the note-spacing stage.

(define-public grob::always-Y-extent-from-stencil
  (ly:make-unpure-pure-container ly:grob::stencil-height))

(define-public (layout-line-thickness grob)
  "Get the line thickness of the @var{grob}'s corresponding layout."
  (let* ((layout (ly:grob-layout grob))
         (line-thickness (ly:output-def-lookup layout 'line-thickness)))

    line-thickness))

(define (grob::objects-from-interface grob iface)
  "For grob @var{grob} return the name and contents of all properties
 within interface @var{iface} having type @code{ly:grob?} or
 @code{ly:grob-array?}."
  (let* ((iface-entry (hashq-ref (ly:all-grob-interfaces) iface))
         (props (if iface-entry (last iface-entry) '()))
         (pointer-props
          (filter
           (lambda (prop)
             (let ((type (object-property prop 'backend-type?)))
               (or (eq? type ly:grob?)
                   (eq? type ly:grob-array?))))
           props)))
    (if (null? pointer-props)
        '()
        (list iface
              (map
               (lambda (prop) (list prop (ly:grob-object grob prop)))
               pointer-props)))))

(define-public (grob::all-objects grob)
  "Return a list of the names and contents of all properties having type
 @code{ly:grob?} or @code{ly:grob-array?} for all interfaces supported by
 grob @var{grob}."
  (let loop ((ifaces (ly:grob-interfaces grob)) (result '()))
    (if (null? ifaces)
        (cons grob (list result))
        (let ((entry (grob::objects-from-interface grob (car ifaces))))
          (if (pair? entry)
              (loop (cdr ifaces) (append result (list entry)))
              (loop (cdr ifaces) result))))))

(use-modules (ice-9 pretty-print))
(define-public (grob::display-objects grob)
  "Display all objects stored in properties of grob @var{grob}."
  (pretty-print (grob::all-objects grob))
  (newline))

(define-public (grob::show-skylines-if-debug-skylines-set grob)
  ;; It looks like the default can be set to this
  ;; value directly, but that wouldn't work when
  ;; using ly:set-option in the file.
  (ly:get-option 'debug-skylines))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beam slope

;; even though kievan noteheads do not have stems, their
;; invisible stems help with beam placement
;; this assures that invisible stems for kievan notes are aligned
;; to the center of kievan noteheads. that is thus where the beams'
;; x extrema will fall
(define-public (stem::kievan-offset-callback grob)
  (let* ((note-heads (ly:grob-object grob 'note-heads #f))
         (note-heads-grobs (if note-heads
                               (ly:grob-array->list note-heads)
                               '()))
         (first-note-head (if (not (null? note-heads-grobs))
                              (car note-heads-grobs)
                              '()))
         (note-head-w (if (not (null? first-note-head))
                          (ly:grob-extent first-note-head first-note-head X)
                          '(0 . 0))))
    (interval-center note-head-w)))


;; sets position of beams for Kievan notation
(define-public (beam::get-kievan-positions grob)
  (let* ((stems (ly:grob-object grob 'stems))
         (stems-grobs (if (not (null? stems))
                          (ly:grob-array->list stems)
                          '()))
         (first-stem (if (not (null? stems-grobs))
                         (car stems-grobs)
                         '()))
         (note-heads (if (not (null? first-stem))
                         (ly:grob-object first-stem 'note-heads)
                         '()))
         (note-heads-grobs (if (not (null? note-heads))
                               (ly:grob-array->list note-heads)
                               '()))
         (first-note-head (if (not (null? note-heads-grobs))
                              (car note-heads-grobs)
                              '()))
         (next-stem (if (not (null? stems))
                        (cadr stems-grobs)
                        '()))
         (next-note-heads (if (not (null? next-stem))
                              (ly:grob-object next-stem 'note-heads)
                              '()))
         (next-note-heads-grobs (if (not (null? next-note-heads))
                                    (ly:grob-array->list next-note-heads)
                                    '()))
         (next-note-head (if (not (null? next-note-heads-grobs))
                             (car next-note-heads-grobs)
                             '()))
         (left-pos (ly:grob-property first-note-head 'Y-offset))
         (right-pos (ly:grob-property next-note-head 'Y-offset))
         (direction (ly:grob-property grob 'direction))
         (first-nh-height (ly:grob::stencil-height first-note-head))
         (next-nh-height (ly:grob::stencil-height next-note-head))
         (left-height (if (= direction DOWN)
                          (+ (car first-nh-height) 0.75)
                          (- (cdr first-nh-height) 0.75)))
         (right-height (if (= direction DOWN)
                           (+ (car next-nh-height) 0.75)
                           (- (cdr next-nh-height) 0.75))))
    (cons (+ left-pos left-height) (+ right-pos right-height))))

(define-public (beam::get-kievan-quantized-positions grob)
  (let* ((pos (ly:grob-property grob 'positions))
         (stems (ly:grob-object grob 'stems #f))
         (stems-grobs (if stems
                          (ly:grob-array->list stems)
                          '())))
    (for-each
     (lambda (g)
       (ly:grob-set-property! g 'stem-begin-position 0)
       (ly:grob-set-property! g 'length 0))
     stems-grobs)
    pos))

;; calculates each slope of a broken beam individually
(define-public (beam::place-broken-parts-individually grob)
  (ly:beam::quanting grob '(+inf.0 . -inf.0) #f))

;; calculates the slope of a beam as a single unit,
;; even if it is broken.  this assures that the beam
;; will pick up where it left off after a line break
(define-public (beam::align-with-broken-parts grob)
  (ly:beam::quanting grob '(+inf.0 . -inf.0) #t))

;; uses the broken beam style from edition peters combines the
;; values of place-broken-parts-individually and align-with-broken-parts above,
;; favoring place-broken-parts-individually when the beam naturally has a steeper
;; incline and align-with-broken-parts when the beam is flat
(define-public (beam::slope-like-broken-parts grob)
  (define (slope y x)
    (/ (- (cdr y) (car y)) (- (cdr x) (car x))))
  (let* ((quant1 (ly:beam::quanting grob '(+inf.0 . -inf.0) #t))
         (original (ly:grob-original grob))
         (siblings (if (ly:grob? original)
                       (ly:spanner-broken-into original)
                       '())))
    (if (null? siblings)
        quant1
        (let* ((quant2 (ly:beam::quanting grob '(+inf.0 . -inf.0) #f))
               (x-span (ly:grob-property grob 'X-positions))
               (slope1 (slope quant1 x-span))
               (slope2 (slope quant2 x-span))
               (quant2 (if (not (= (sign slope1) (sign slope2)))
                           '(0 . 0)
                           quant2))
               (factor (/ (atan (abs slope1)) PI-OVER-TWO))
               (base (offset-add
                      (offset-scale quant1 (- 1 factor))
                      (offset-scale quant2 factor))))
          (ly:beam::quanting grob base #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cross-staff stuff

(define-public (script-or-side-position-cross-staff g)
  (or
   (ly:script-interface::calc-cross-staff g)
   (ly:side-position-interface::calc-cross-staff g)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; side-position stuff

(define-public (only-if-beamed g)
  (any (lambda (x) (ly:grob? (ly:grob-object x 'beam)))
       (ly:grob-array->list (ly:grob-object g 'side-support-elements))))

(define-public side-position-interface::y-aligned-side
  (ly:make-unpure-pure-container
   ly:side-position-interface::y-aligned-side
   ly:side-position-interface::pure-y-aligned-side))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; break-alignable stuff

(define-public (break-alignable-interface::self-alignment-of-anchor g)
  "Return a value for @var{g}'s @code{self-alignment-X} that will
   place @var{g} on the same side of the reference point defined by a
   @code{break-aligned} item such as a @code{Clef}."
  (let ((parent (ly:break-alignable-interface::find-parent g)))
    (if (ly:grob? parent)
        (ly:grob-property parent 'break-align-anchor-alignment CENTER)
        CENTER)))

(define-public (break-alignable-interface::self-alignment-opposite-of-anchor g)
  "Return a value for @var{g}'s @code{self-alignment-X} that will
   place @var{g} on the opposite side of the reference point defined by a
   @code{break-aligned} item such as a @code{Clef}."
  (* -1 (break-alignable-interface::self-alignment-of-anchor g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; self-alignment stuff

(define-public self-alignment-interface::y-aligned-on-self
  (ly:make-unpure-pure-container
   ly:self-alignment-interface::y-aligned-on-self
   ly:self-alignment-interface::pure-y-aligned-on-self))

(define-public (self-alignment-interface::self-aligned-on-breakable grob)
  "Return the @code{X-offset} that places @var{grob} according to its
   @code{self-alignment-X} over the reference point defined by the
   @code{break-align-anchor-alignment} of a @code{break-aligned} item
   such as a @code{Clef}."
  (+ (ly:break-alignable-interface::self-align-callback grob)
     (ly:self-alignment-interface::x-aligned-on-self grob)))

(define-public (break-alignment-list end-of-line middle begin-of-line)
  "Return a callback that calculates a value based on a grob's break
  direction."
  (lambda (grob)
    (let ((rval (case (ly:item-break-dir grob)
                  ((1) begin-of-line)
                  ((0) middle)
                  ((-1) end-of-line))))
      (if (procedure? rval)
          (rval grob)
          rval))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; staff ellipsis

(define-public (staff-ellipsis::calc-y-extent grob)
  "Callback for @code{StaffEllipsis} grob, which is used with
@code{skipTypesetting}."
  (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
         (staff-extent (if (ly:grob? staff-symbol)
                           (ly:grob-extent staff-symbol staff-symbol Y)
                           (cons 0 0))))
    ;; widen slightly to cover the outer staff lines even with rounding error
    (interval-widen staff-extent 1/32)))

(define-public (staff-ellipsis::print grob)
  "Callback for @code{StaffEllipsis} grob, which is used with
@code{skipTypesetting}."
  (let ((text-sten (grob-interpret-markup grob (ly:grob-property grob 'text))))
    (ly:make-stencil (ly:stencil-expr text-sten)
                     (ly:stencil-extent text-sten X)
                     (ly:grob-extent grob grob Y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; staff symbol

(define-public staff-symbol-referencer::callback
  (ly:make-unpure-pure-container ly:staff-symbol-referencer::callback))

(define-public (staff-symbol::calc-widened-extent grob)
  (let* ((basic-staff-space (ly:staff-symbol-staff-space grob))
         ;; DOCME: unclear what this is for.
         (staff-space (if (zero? basic-staff-space)
                          1.0
                          basic-staff-space))
         (ext (ly:grob-extent grob grob Y)))
    (if (< (interval-length ext)
           (* staff-space 2))
        ;; Avoid bar lines shorter than two staff spaces.
        ;; (Gould seems to use 4 spaces, judging from her
        ;; examples.)  Cope by extending the bar line by one
        ;; staff space in each direction.
        (interval-widen ext staff-space)
        ext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note heads

(define-public (stem::calc-duration-log grob)
  (ly:duration-log
   (ly:event-property (event-cause grob) 'duration)))

(define (stem-stub::do-calculations grob)
  (and (ly:grob-property (ly:grob-parent grob X) 'cross-staff)
       (not (ly:grob-property (ly:grob-parent grob X) 'transparent))))

(define-public (stem-stub::pure-height grob beg end)
  (if (stem-stub::do-calculations grob)
      '(0 . 0)
      '(+inf.0 . -inf.0)))

(define-public (stem-stub::width grob)
  (if (stem-stub::do-calculations grob)
      (grob::x-parent-width grob)
      '(+inf.0 . -inf.0)))

(define-public (stem-stub::extra-spacing-height grob)
  (if (stem-stub::do-calculations grob)
      (let* ((dad (ly:grob-parent grob X))
             (refp (ly:grob-common-refpoint grob dad Y))
             (stem_ph (ly:grob-pure-height dad refp 0 INFINITY-INT))
             (my_ph (ly:grob-pure-height grob refp 0 INFINITY-INT))
             ;; only account for distance if stem is on different staff than stub
             (dist (if (grob::has-interface refp 'hara-kiri-group-spanner-interface)
                       0
                       (- (car my_ph) (car stem_ph)))))
        (if (interval-empty? (interval-intersection stem_ph my_ph)) #f (coord-translate stem_ph dist)))
      #f))

(define-public (note-head::calc-kievan-duration-log grob)
  (min 3
       (ly:duration-log
        (ly:event-property (event-cause grob) 'duration))))

(define-public (note-head::calc-duration-log grob)
  (min 2
       (ly:duration-log
        (ly:event-property (event-cause grob) 'duration))))

(define-public (dots::calc-dot-count grob)
  (ly:duration-dot-count
   (ly:event-property (event-cause grob) 'duration)))

(define-public (dots::calc-staff-position grob)
  (let* ((head (ly:grob-parent grob Y))
         (log (ly:grob-property head 'duration-log)))

    (if (or (not (grob::has-interface head 'rest-interface))
            (not (integer? log)))
        0
        (case log
          ((0)   -2)
          ((5 6)  2)
          ((7 8)  4)
          ((9 10) 6)
          (else   0)))))

(define-public (dots::calc-glyph-name grob)
  (let ((style (ly:grob-property grob 'style)))
    (format #f "dots.dot~a"
            (if (symbol? style)
                (symbol->string style)
                ""))))

(define-public (dots::calc-dot-stencil grob)
  (let* ((font (ly:grob-default-font grob))
         (name (ly:grob-property grob 'glyph-name))
         (stencil (ly:font-get-glyph font name)))
    (if (ly:stencil-empty? stencil)
        (begin
          (ly:grob-warning grob
                           #f
                           (G_ "dot glyph ~s not found")
                           name)
          point-stencil)
        stencil)))

(define-public (ly:dots::print grob)
  (let* ((dot-stencil (ly:grob-property grob 'dot-stencil))
         (padding (interval-length (ly:stencil-extent dot-stencil X)))
         (count (ly:grob-property grob 'dot-count 0)))
    (stack-stencils X RIGHT padding (make-list count dot-stencil))))

(define-public (dot-column-interface::pad-by-one-dot-width grob)
  ;; The default for padding dots is to pad by the width of one dot exactly
  ;; (this width depends on the dot style used).  We do a little better than
  ;; assuming all dots have the same width by taking the max of all widths.
  (apply max
         (map
          (lambda (d)
            (if (grob::is-live? d)
                (let* ((dot-stencil (ly:grob-property d 'dot-stencil))
                       (ext (ly:stencil-extent dot-stencil X)))
                  (interval-length ext))
                0.0))
          (ly:grob-array->list (ly:grob-object grob 'dots)))))

;; Kept separate from note-head::calc-glyph-name to allow use by
;; markup commands \note and \note-by-number
(define-public (select-head-glyph style log)
  "Select a note head glyph string based on note head style @var{style}
and duration log @var{log}."
  (if (symbol? style)
      (case style
        ;; "default" style is directly handled in note-head.cc as a
        ;; special case (HW says, mainly for performance reasons).
        ;; Therefore, style "default" does not appear in this case
        ;; statement.  -- jr
        ;; Though we not to care if style is '(), see below.  -- harm
        ((xcircle) "2xcircle")
        ((harmonic) "0harmonic")
        ((harmonic-black) "2harmonic")
        ((harmonic-mixed) (if (<= log 1) "0harmonic"
                              "2harmonic"))
        ((baroque)
         ;; Oops, I actually would not call this "baroque", but, for
         ;; backwards compatibility to 1.4, this is supposed to take
         ;; brevis, longa and maxima from the neo-mensural font and all
         ;; other note heads from the default font.  -- jr
         (if (< log 0)
             (string-append (number->string log) "neomensural")
             (number->string log)))
        ((altdefault)
         ;; Like default, but brevis is drawn with double vertical lines
         (if (= log -1)
             (string-append (number->string log) "double")
             (number->string log)))
        ((mensural)
         (string-append (number->string log) (symbol->string style)))
        ((petrucci)
         (if (< log 0)
             (string-append (number->string log) "mensural")
             (string-append (number->string log) (symbol->string style))))
        ((blackpetrucci)
         (if (< log 0)
             (string-append (number->string log) "blackmensural")
             (string-append (number->string log) (symbol->string style))))
        ((semipetrucci)
         (if (< log 0)
             (string-append (number->string log) "semimensural")
             (string-append (number->string log) "petrucci")))
        ((neomensural)
         (string-append (number->string log) (symbol->string style)))
        ((kievan)
         (string-append (number->string log) "kievan"))
        (else
         (if (string-match "vaticana*|hufnagel*|medicaea*"
                           (symbol->string style))
             (symbol->string style)
             (string-append (number->string (max 0 log))
                            (symbol->string style)))))
      ;; 'vaticana-ligature-interface has a 'glyph-name-property for NoteHead.
      ;; Probably best to return an empty list here, if called in a context
      ;; without setting 'style, i.e. 'style is '(), to avoid a scheme-error.
      '()))

(define-public (note-head::calc-glyph-name grob)
  (let* ((style (ly:grob-property grob 'style))
         (log (min (if (eq? 'kievan style) 3 2) (ly:grob-property grob 'duration-log))))
    (select-head-glyph style log)))

(define-public (note-head::brew-ez-stencil grob)
  (let* ((log (ly:grob-property grob 'duration-log))
         (pitch (ly:event-property (event-cause grob) 'pitch))
         (pitch-index (ly:pitch-notename pitch))
         (note-names (ly:grob-property grob 'note-names))
         (pitch-string (if (and (vector? note-names)
                                (> (vector-length note-names) pitch-index))
                           (vector-ref note-names pitch-index)
                           (string
                            (integer->char
                             (+ (modulo (+ pitch-index 2) 7)
                                (char->integer #\A))))))
         (staff-space (ly:staff-symbol-staff-space grob))
         (line-thickness (ly:staff-symbol-line-thickness grob))
         (stem (ly:grob-object grob 'stem #f))
         (stem-thickness (* (if stem
                                (ly:grob-property stem 'thickness)
                                1.3)
                            line-thickness))
         (font-size (ly:grob-property grob 'font-size 0))
         (radius (* (magstep font-size) (/ (+ staff-space line-thickness) 2)))
         (letter (make-fontsize-markup
                  -8
                  (make-center-align-markup (make-vcenter-markup pitch-string))))
         (filled-circle (make-draw-circle-markup radius 0 #t)))

    (ly:stencil-translate-axis
     (grob-interpret-markup
      grob
      (if (>= log 2)
          (make-combine-markup
           filled-circle
           (make-with-color-markup white letter))
          (make-combine-markup
           (make-combine-markup
            filled-circle
            (make-with-color-markup white (make-draw-circle-markup
                                           (- radius stem-thickness) 0 #t)))
           letter)))
     radius X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clipping

(define-public (make-rhythmic-location bar-num num den)
  (cons
   bar-num (ly:make-moment num den)))

(define-public (rhythmic-location? a)
  (and (pair? a)
       (integer? (car a))
       (ly:moment? (cdr a))))

(define-public (make-graceless-rhythmic-location loc)
  (make-rhythmic-location
   (car loc)
   (ly:moment-main-numerator (rhythmic-location-measure-position loc))
   (ly:moment-main-denominator (rhythmic-location-measure-position loc))))

(define-public rhythmic-location-measure-position cdr)
(define-public rhythmic-location-bar-number car)

(define-public (rhythmic-location<? a b)
  (cond
   ((< (car a) (car b)) #t)
   ((> (car a) (car b)) #f)
   (else
    (ly:moment<? (cdr a) (cdr b)))))

(define-public (rhythmic-location<=? a b)
  (not (rhythmic-location<? b a)))
(define-public (rhythmic-location>=? a b)
  (not (rhythmic-location<? a b)))
(define-public (rhythmic-location>? a b)
  (rhythmic-location<? b a))

(define-public (rhythmic-location=? a b)
  (and (rhythmic-location<=? a b)
       (rhythmic-location<=? b a)))

(define-public (rhythmic-location->file-string a)
  (format #f "~a.~a.~a"
          (car a)
          (ly:moment-main-numerator (cdr a))
          (ly:moment-main-denominator (cdr a))))

(define-public (rhythmic-location->string a)
  (format #f "bar ~a ~a"
          (car a)
          (ly:moment->string (cdr a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; break visibility

(define-public all-visible             #(#t #t #t))
(define-public begin-of-line-invisible #(#t #t #f))
(define-public center-invisible        #(#t #f #t))
(define-public end-of-line-invisible   #(#f #t #t))
(define-public begin-of-line-visible   #(#f #f #t))
(define-public center-visible          #(#f #t #f))
(define-public end-of-line-visible     #(#t #f #f))
(define-public all-invisible           #(#f #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a generic extra-spacing-height routine

(define-public (item::extra-spacing-height-including-staff grob)
  "Return a value for @code{extra-spacing-height} that augments the
extent of the grob to the extent of the staff."
  (let ((staff-symbol (ly:grob-object grob 'staff-symbol #f)))
    (if staff-symbol
        (let* ((common (ly:grob-common-refpoint grob staff-symbol Y))
               (my-ext (ly:grob-extent grob common Y))
               (staff-ext (ly:grob-extent staff-symbol common Y))
               (to-staff (pair-map - staff-ext my-ext)))
          (pair-map (cons min max) '(0 . 0) to-staff))
        '(0 . 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neighbor-interface routines


(define-public (shift-right-at-line-begin g)
  "Shift an item to the right, but only at the start of the line."
  (if (and (ly:item? g)
           (equal? (ly:item-break-dir g) RIGHT))
      (ly:grob-translate-axis! g 3.5 X)))

(define-public (pure-from-neighbor-interface::extra-spacing-height-at-beginning-of-line grob)
  (if (= 1 (ly:item-break-dir grob))
      (pure-from-neighbor-interface::extra-spacing-height grob)
      (cons -0.1 0.1)))

(define-public (pure-from-neighbor-interface::extra-spacing-height grob)
  (let* ((height (ly:grob-pure-height grob grob 0 INFINITY-INT))
         (from-neighbors (interval-union
                          height
                          (ly:axis-group-interface::pure-height
                           grob
                           0
                           INFINITY-INT))))
    (pair-map - from-neighbors height)))

;; If there are neighbors, we place the height at their midpoint
;; to avoid protrusion of this pure height out of the vertical
;; axis group on either side.  This will minimize the impact of the
;; grob on pure minimum translations.

;; TODO - there is a double call to axis-group-interface::pure-height
;; here and then in the extra-spacing-height function above. Can/should this
;; be rolled into one?
(define-public (pure-from-neighbor-interface::pure-height grob beg end)
  (let* ((height (ly:axis-group-interface::pure-height
                  grob
                  0
                  INFINITY-INT))
         (c (interval-center height)))
    (if (interval-empty? height) empty-interval (cons c c))))

;; Minimizes the impact of the height on vertical spacing while allowing
;; it to appear in horizontal skylines of paper columns if necessary.
(define-public pure-from-neighbor-interface::height-if-pure
  (ly:make-unpure-pure-container #f pure-from-neighbor-interface::pure-height))

(define-public (pure-from-neighbor-interface::account-for-span-bar grob)
  (let* ((esh (pure-from-neighbor-interface::extra-spacing-height grob))
         (hsb (ly:grob-object grob 'has-span-bar))
         (ii (interval-intersection esh (cons -1.01 1.01))))
    (if (pair? hsb)
        (cons (car (if (and (car hsb)
                            (ly:grob-property grob 'allow-span-bar))
                       esh ii))
              (cdr (if (cdr hsb) esh ii)))
        ii)))

(define-public (pure-from-neighbor-interface::extra-spacing-height-including-staff grob)
  (pair-map (cons min max)
            (pure-from-neighbor-interface::extra-spacing-height grob)
            (item::extra-spacing-height-including-staff grob)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tuplets

(define-public (tuplet-number::calc-direction grob)
  (ly:tuplet-bracket::calc-direction (ly:grob-object grob 'bracket)))

(define-public (tuplet-number::calc-denominator-text grob)
  (number->string (ly:event-property (event-cause grob) 'denominator)))

(define-public (tuplet-number::calc-fraction-text grob)
  (let ((ev (event-cause grob)))

    (format #f "~a:~a"
            (ly:event-property ev 'denominator)
            (ly:event-property ev 'numerator))))

;; a formatter function, which is simply a wrapper around an existing
;; tuplet formatter function. It takes the value returned by the given
;; function and appends a note of given length.
(define-public ((tuplet-number::append-note-wrapper function note) grob)
  (let ((txt (and function (function grob))))

    (if txt
        (make-line-markup
         (list txt (make-fontsize-markup -5 (make-note-markup note UP))))
        (make-fontsize-markup -5 (make-note-markup note UP)))))

;; Print a tuplet denominator with a different number than the one derived from
;; the actual tuplet fraction
(define-public ((tuplet-number::non-default-tuplet-denominator-text denominator)
                grob)
  (number->string (if denominator
                      denominator
                      (ly:event-property (event-cause grob) 'denominator))))

;; Print a tuplet fraction with different numbers than the ones derived from
;; the actual tuplet fraction
(define-public ((tuplet-number::non-default-tuplet-fraction-text
                 denominator numerator) grob)
  (let* ((ev (event-cause grob))
         (den (if denominator denominator (ly:event-property ev 'denominator)))
         (num (if numerator numerator (ly:event-property ev 'numerator))))

    (format #f "~a:~a" den num)))

;; Print a tuplet fraction with note durations appended to the numerator and the
;; denominator
(define-public ((tuplet-number::fraction-with-notes
                 denominatornote numeratornote) grob)
  (let* ((ev (event-cause grob))
         (denominator (ly:event-property ev 'denominator))
         (numerator (ly:event-property ev 'numerator)))

    ((tuplet-number::non-default-fraction-with-notes
      denominator denominatornote numerator numeratornote) grob)))

;; Print a tuplet fraction with note durations appended to the numerator and the
;; denominator
(define-public ((tuplet-number::non-default-fraction-with-notes
                 denominator denominatornote numerator numeratornote) grob)
  (let* ((ev (event-cause grob))
         (den (if denominator denominator (ly:event-property ev 'denominator)))
         (num (if numerator numerator (ly:event-property ev 'numerator))))

    (make-concat-markup (list
                         (make-simple-markup (format #f "~a" den))
                         (make-fontsize-markup -5 (make-note-markup denominatornote UP))
                         (make-simple-markup " : ")
                         (make-simple-markup (format #f "~a" num))
                         (make-fontsize-markup -5 (make-note-markup numeratornote UP))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color

(define-public (color? x)
  (or
   (string? x)
   (and (list? x)
        (let ((l (length x)))
          (or (= 3 l)
              (= 4 l)))
        (every number? x)
        (every (lambda (y) (<= 0 y 1)) x))))

(define*-public (rgb-color r g b #:optional (a #f))
  (if a
      (list r g b a)
      (list r g b)))

;; predefined colors
(define-public black       '(0.0 0.0 0.0))
(define-public white       '(1.0 1.0 1.0))
(define-public red         '(1.0 0.0 0.0))
(define-public green       '(0.0 1.0 0.0))
(define-public blue        '(0.0 0.0 1.0))
(define-public cyan        '(0.0 1.0 1.0))
(define-public magenta     '(1.0 0.0 1.0))
(define-public yellow      '(1.0 1.0 0.0))

(define-public grey        '(0.5 0.5 0.5))
(define-public darkred     '(0.5 0.0 0.0))
(define-public darkgreen   '(0.0 0.5 0.0))
(define-public darkblue    '(0.0 0.0 0.5))
(define-public darkcyan    '(0.0 0.5 0.5))
(define-public darkmagenta '(0.5 0.0 0.5))
(define-public darkyellow  '(0.5 0.5 0.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key signature

(define-public (key-signature-interface::alteration-positions
                entry c0-position grob)
  (let ((step (car entry))
        (alter (cdr entry)))
    (if (pair? step)
        (list (+ (cdr step) (* (car step) 7) c0-position))
        (let* ((c-position (modulo c0-position 7))
               (positions
                (if (< alter 0)
                    ;; See (flat|sharp)-positions in define-grob-properties.scm
                    (ly:grob-property grob 'flat-positions '(3))
                    (ly:grob-property grob 'sharp-positions '(3))))
               (p (list-ref positions
                            (if (< c-position (length positions))
                                c-position 0)))
               (max-position (if (pair? p) (cdr p) p))
               (min-position (if (pair? p) (car p) (- max-position 6)))
               (first-position (+ (modulo (- (+ c-position step)
                                             min-position)
                                          7)
                                  min-position)))
          (define (prepend x l) (if (> x max-position)
                                    l
                                    (prepend (+ x 7) (cons x l))))
          (prepend first-position '())))))

(define-public (key-signature-interface::alteration-position
                step alter c0-position)
  ;; Deprecated.  Not a documented interface, and no longer used in LilyPond,
  ;; but needed for a popular file, LilyJAZZ.ily for version 2.16
  (if (pair? step)
      (+ (cdr step) (* (car step) 7) c0-position)
      (let* ((c-pos (modulo c0-position 7))
             (hi (list-ref
                  (if (< alter 0)
                      '(2 3 4 2 1 2 1) ; position of highest flat
                      '(4 5 4 2 3 2 3)); position of highest sharp
                  c-pos)))
        (- hi (modulo (- hi (+ c-pos step)) 7)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotations

(define-public (numbered-footnotes int)
  (make-tiny-markup (number->string (+ 1 int))))

(define-public (symbol-footnotes int)
  (define (helper symbols out idx n)
    (if (< n 1)
        out
        (helper symbols
                (string-append out (list-ref symbols idx))
                idx
                (- n 1))))
  (make-tiny-markup (helper '("*" "†" "‡" "§" "¶")
                            ""
                            (remainder int 5)
                            (+ 1 (quotient int 5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accidentals

(define-public (accidental-interface::calc-alteration grob)
  (ly:pitch-alteration (ly:event-property (event-cause grob) 'pitch)))

(define-public (accidental-interface::calc-glyph-name grob)
  (let* ((alteration (ly:grob-property grob 'alteration))
         (layout (ly:grob-layout grob))
         (defs (ly:output-def-lookup layout 'font-defaults '()))
         (chain (ly:grob-alist-chain grob defs))
         (alist (chain-assoc-get 'alteration-glyph-name-alist chain '()))
         (glyph (assv-ref alist alteration)))
    (or glyph
        (begin
          (ly:warning (G_ "no glyph name for alteration ~a in \
alteration-glyph-name-alist; try defining one in alterationGlyphs")
                      alteration)
          "noteheads.s1cross"))))

(define-public accidental-interface::height
  (ly:make-unpure-pure-container
   ly:accidental-interface::height))

(define-public standard-alteration-glyph-name-alist
  '(
    ;; ordered for optimal performance.
    (0 . "accidentals.natural")
    (-1/2 . "accidentals.flat")
    (1/2 . "accidentals.sharp")

    (1 . "accidentals.doublesharp")
    (-1 . "accidentals.flatflat")

    (3/4 . "accidentals.sharp.slashslash.stemstemstem")
    (1/4 . "accidentals.sharp.slashslash.stem")
    (-1/4 . "accidentals.mirroredflat")
    (-3/4 . "accidentals.mirroredflat.flat")))

;; FIXME: standard vs default, alteration-FOO vs FOO-alteration
(define-public alteration-default-glyph-name-alist
  standard-alteration-glyph-name-alist)

(define-public makam-alteration-glyph-name-alist
  '((1 . "accidentals.doublesharp")
    (8/9 . "accidentals.sharp.slashslashslash.stemstem")
    (5/9 . "accidentals.sharp.slashslashslash.stem")
    (4/9 . "accidentals.sharp")
    (1/9 . "accidentals.sharp.slashslash.stem")
    (0 . "accidentals.natural")
    (-1/9 . "accidentals.mirroredflat")
    (-4/9 . "accidentals.flat.slash")
    (-5/9 . "accidentals.flat")
    (-8/9 . "accidentals.flat.slashslash")
    (-1 . "accidentals.flatflat")))

(define-public alteration-hufnagel-glyph-name-alist
  '((-1/2 . "accidentals.hufnagelM1")
    (0 . "accidentals.vaticana0")
    (1/2 . "accidentals.mensural1")))

(define-public alteration-medicaea-glyph-name-alist
  '((-1/2 . "accidentals.medicaeaM1")
    (0 . "accidentals.vaticana0")
    (1/2 . "accidentals.mensural1")))

(define-public alteration-vaticana-glyph-name-alist
  '((-1/2 . "accidentals.vaticanaM1")
    (0 . "accidentals.vaticana0")
    (1/2 . "accidentals.mensural1")))

(define-public alteration-mensural-glyph-name-alist
  '((-1/2 . "accidentals.mensuralM1")
    (0 . "accidentals.vaticana0")
    (1/2 . "accidentals.mensural1")))

(define-public alteration-kievan-glyph-name-alist
  '((-1/2 . "accidentals.kievanM1")
    (1/2 . "accidentals.kievan1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Pitch Trill Heads
;; * Parentheses

(define-public (parentheses-interface::calc-parenthesis-stencils grob)
  (let* ((font (ly:grob-default-font grob))
         (lp (ly:font-get-glyph font "accidentals.leftparen"))
         (rp (ly:font-get-glyph font "accidentals.rightparen")))

    (list lp rp)))

(define-public (parentheses-interface::calc-angled-bracket-stencils grob)
  (let* ((parent (ly:grob-parent grob Y))
         (y-extent (ly:grob-extent parent parent Y))
         (half-thickness 0.05) ; should it be a property?
         (width 0.5) ; should it be a property?
         (angularity 1.5)  ; makes angle brackets
         (white-padding 0.1) ; should it be a property?
         (lp (ly:stencil-aligned-to
              (ly:stencil-aligned-to
               (make-parenthesis-stencil y-extent
                                         half-thickness
                                         width
                                         angularity
                                         -1)
               Y CENTER)
              X RIGHT))
         (lp-x-extent
          (interval-widen (ly:stencil-extent lp X) white-padding))
         (rp (ly:stencil-aligned-to
              (ly:stencil-aligned-to
               (make-parenthesis-stencil y-extent
                                         half-thickness
                                         width
                                         angularity
                                         1)
               Y CENTER)
              X LEFT))
         (rp-x-extent
          (interval-widen (ly:stencil-extent rp X) white-padding)))
    (set! lp (ly:make-stencil (ly:stencil-expr lp)
                              lp-x-extent
                              (ly:stencil-extent lp Y)))
    (set! rp (ly:make-stencil (ly:stencil-expr rp)
                              rp-x-extent
                              (ly:stencil-extent rp Y)))
    (list (stencil-whiteout-box lp)
          (stencil-whiteout-box rp))))

(define-public (parentheses-interface::y-extent grob) (ly:grob::stencil-height grob))

(define-public (parenthesize-elements grob)
  (let* ((elts (ly:grob-array->list (ly:grob-object grob 'elements)))
         (get-friends
          (lambda (g)
            (let ((syms (ly:grob-property g 'parenthesis-friends '()))
                  (get-friend (lambda (s)
                                (let ((f (ly:grob-object g s)))
                                  (cond
                                   ((ly:grob? f) (list f))
                                   ((ly:grob-array? f) (ly:grob-array->list f))
                                   (else '()))))))
              (apply append (map get-friend syms)))))
         (parenthesized-elts
          (ly:grob-list->grob-array
           (apply append elts (map get-friends elts))))
         (refp (ly:grob-common-refpoint-of-array grob parenthesized-elts X))
         (x-ext (ly:relative-group-extent parenthesized-elts refp X))
         (padding (ly:grob-property grob 'padding 0.1))
         (wide-ext (interval-widen x-ext padding))
         (my-x (ly:grob-relative-coordinate grob refp X))
         (parenthesis-positions (coord-translate wide-ext (- my-x)))
         (stencils (ly:grob-property grob 'stencils))
         (lp (car stencils))
         (rp (cadr stencils)))
    (ly:stencil-add
     (ly:stencil-translate-axis lp (interval-start parenthesis-positions) X)
     (ly:stencil-translate-axis rp (interval-end parenthesis-positions) X))))

(define-public (parentheses-interface::print grob)
  (let* ((y-parent (ly:grob-parent grob Y))
         ;; The Y alignment is based on elements and not parenthesized-elements.
         ;; We don't want the friends, or parenthesized notes with a flat would
         ;; look bad.
         (elts (ly:grob-object grob 'elements))
         (refp (ly:grob-common-refpoint-of-array grob elts Y))
         (stencil (parenthesize-elements grob))
         (y-ext (ly:relative-group-extent elts refp Y))
         (y-center (interval-center y-ext))
         (my-y (ly:grob-relative-coordinate grob refp Y))
         (translation (- y-center my-y)))
    (ly:stencil-translate-axis stencil translation Y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; offset callbacks

(define-public (pure-chain-offset-callback grob start end prev-offset)
  "Sometimes, a chained offset callback is unpure and there is
   no way to write a pure function that estimates its behavior.
   In this case, we use a pure equivalent that will simply pass
   the previous calculated offset value."
  prev-offset)

(define-public (scale-by-font-size x)
  (ly:make-unpure-pure-container
   (lambda (grob)
     (* x (magstep (ly:grob-property grob 'font-size 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define-public (grob::compose-function func data)
  "Create a callback entity @var{func} to be stored in a grob property,
based on the grob property data @var{data} (which can be plain data, a
callback itself, or an unpure-pure container).

Function or unpure-pure container @var{func} accepts a grob and a
value and returns another value.  Depending on the type of @var{data},
@var{func} is used for building a grob callback or an
unpure-pure container."
  (if (or (ly:unpure-pure-container? func)
          (ly:unpure-pure-container? data))
      (ly:make-unpure-pure-container
       (lambda (grob) (ly:unpure-call func grob (ly:unpure-call data grob)))
       (lambda (grob start end)
         (ly:pure-call func grob start end
                       (ly:pure-call data grob start end))))
      (lambda (grob) (ly:unpure-call func grob (ly:unpure-call data grob)))))

;; Don't use define* since then we can't start the body with define in
;; Guile-1.8: crazy bug.
(define-public (grob::offset-function func data . rest)
  "Create a callback entity @var{func} to be stored in a grob property,
based on the grob property data @var{data} (which can be plain data, a
callback itself, or an unpure-pure container).

Function @var{func} accepts a grob and returns a value that is added
to the value resulting from @var{data}.  Optional argument @var{plus}
defaults to @samp{+} but may be changed to allow for using a different
underlying accumulation.

If @var{data} is @code{#f} or @code{'()}, it is not included in the sum."
  (define plus (if (null? rest) + (car rest)))
  (define (maybeplus a b)
    (if (or (not b) (null? b)) a (plus a b)))
  (cond ((or (not data) (null? data))
         func)
        ((or (ly:unpure-pure-container? func)
             (ly:unpure-pure-container? data))
         (ly:make-unpure-pure-container
          (lambda rest
            (maybeplus (apply ly:unpure-call func rest)
                       (apply ly:unpure-call data rest)))
          (lambda rest
            (maybeplus (apply ly:pure-call func rest)
                       (apply ly:pure-call data rest)))))
        ((or (procedure? func)
             (procedure? data))
         (lambda rest
           (maybeplus (apply ly:unpure-call func rest)
                      (apply ly:unpure-call data rest))))
        (else (plus func data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; falls/doits

(define-public (bend::print spanner)
  (define (close  a b)
    (< (abs (- a b)) 0.01))

  (let* ((delta-y (* 0.5 (ly:grob-property spanner 'delta-position)))
         (left-span (ly:spanner-bound spanner LEFT))
         (dots (if (and (grob::has-interface left-span 'note-head-interface)
                        (ly:grob-object left-span 'dot #f))
                   (ly:grob-object left-span 'dot) #f))

         (right-span (ly:spanner-bound spanner RIGHT))
         (thickness (* (ly:grob-property spanner 'thickness)
                       (ly:output-def-lookup (ly:grob-layout spanner)
                                             'line-thickness)))
         (padding (ly:grob-property spanner 'padding 0.5))
         (common (ly:grob-common-refpoint right-span
                                          (ly:grob-common-refpoint spanner
                                                                   left-span X)
                                          X))
         (common-y (ly:grob-common-refpoint spanner left-span Y))
         (minimum-length (ly:grob-property spanner 'minimum-length 0.5))

         (left-x (+ padding
                    (max
                     (interval-end (ly:generic-bound-extent
                                    left-span common))
                     (if
                      (and dots
                           (close
                            (ly:grob-relative-coordinate dots common-y Y)
                            (ly:grob-relative-coordinate spanner common-y Y)))
                      (interval-end
                       (ly:grob-robust-relative-extent dots common X))
                      (- INFINITY-INT)))))
         (right-x (max (- (interval-start
                           (ly:generic-bound-extent right-span common))
                          padding)
                       (+ left-x minimum-length)))
         (self-x (ly:grob-relative-coordinate spanner common X))
         (dx (- right-x left-x))
         (exp (list 'path thickness
                    `(rmoveto
                      ,(- left-x self-x) 0

                      rcurveto
                      ,(/ dx 3)
                      0
                      ,dx ,(* 0.66 delta-y)
                      ,dx ,delta-y))))

    (ly:make-stencil
     exp
     (cons (- left-x self-x) (- right-x self-x))
     (cons (min 0 delta-y)
           (max 0 delta-y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grace spacing

(define-public (grace-spacing::calc-shortest-duration grob)
  (let* ((cols (ly:grob-object grob 'columns))
         (get-difference
          (lambda (idx)
            (ly:moment-sub (ly:grob-property
                            (ly:grob-array-ref cols (1+ idx)) 'when)
                           (ly:grob-property
                            (ly:grob-array-ref cols idx) 'when))))

         (moment-min (lambda (x y)
                       (cond
                        ((and x y)
                         (if (ly:moment<? x y)
                             x
                             y))
                        (x x)
                        (y y)))))

    (fold moment-min #f (map get-difference
                             (iota (1- (ly:grob-array-length cols)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fingering

(define-public (fingering::calc-text grob)
  (let ((event (event-cause grob)))
    (or (ly:event-property event 'text #f)
        (number->string (ly:event-property event 'digit) 10))))

(define-public (string-number::calc-text grob)
  (let ((event (event-cause grob)))
    (or (ly:event-property event 'text #f)
        (number-format
         (ly:grob-property grob 'number-type)
         (ly:event-property event 'string-number)))))

(define-public (stroke-finger::calc-text grob)
  (let ((event (event-cause grob)))
    (or (ly:event-property event 'text #f)
        (let ((digit-names (ly:grob-property grob 'digit-names)))
          (vector-ref digit-names
                      (1- (max 1
                               (min (vector-length digit-names)
                                    (ly:event-property event 'digit)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dynamics

(define-public (hairpin::calc-grow-direction grob)
  (if (ly:in-event-class? (event-cause grob) 'decrescendo-event)
      START
      STOP))

(define-public (dynamic-text-spanner::before-line-breaking grob)
  "Monitor left bound of @code{DynamicTextSpanner} for absolute dynamics.
If found, ensure @code{DynamicText} does not collide with spanner text by
changing @code{'attach-dir} and @code{'padding}.  Reads the
@code{'right-padding} property of @code{DynamicText} to fine-tune space
between the two text elements."
  (let ((left-bound (ly:spanner-bound grob LEFT)))
    (if (grob::has-interface left-bound 'dynamic-text-interface)
        (let* ((details (ly:grob-property grob 'bound-details))
               (left-details (ly:assoc-get 'left details))
               (my-padding (ly:assoc-get 'padding left-details))
               (script-padding (ly:grob-property left-bound 'right-padding 0)))

          (and (number? my-padding)
               (ly:grob-set-nested-property! grob
                                             '(bound-details left attach-dir)
                                             RIGHT)
               (ly:grob-set-nested-property! grob
                                             '(bound-details left padding)
                                             (+ my-padding script-padding)))))))

(define-public (make-connected-line points grob)
  "Take a list of points, @var{points}.
Return a line connecting @var{points},
using @code{ly:@/line-interface::@/line} and getting layout information from
@var{grob}."
  (define (connected-points grob ls pts)
    (if (not (pair? (cdr pts)))
        (reduce ly:stencil-add empty-stencil ls)
        (connected-points
         grob
         (cons
          (ly:line-interface::line
           grob
           (car (first pts))
           (cdr (first pts))
           (car (second pts))
           (cdr (second pts)))
          ls)
         (cdr pts))))
  (if (< (length points) 2)
      (begin
        (ly:warning
         "'make-connected-line' needs at least two points: ~a"
         points)
        empty-stencil)
      (connected-points grob '() points)))

(define-public ((elbowed-hairpin coords mirrored?) grob)
  "Create hairpin based on a list of @var{coords} in @code{(cons x y)}
form.  @code{x}@tie{}is the portion of the width consumed for a given line
and @code{y}@tie{}is the portion of the height.  For example,
@code{'((0 . 0) (0.3 . 0.7) (0.8 . 0.9) (1.0 . 1.0))} means that at the point
where the hairpin has consumed 30% of its width, it must
be at 70% of its height.  Once it is to 80% width, it
must be at 90% height.  It finishes at 100% width and 100% height.
If @var{coords} does not begin with @code{'(0 . 0)} the final hairpin may have
an open tip.  For example '(0 . 0.5) will cause an open end of 50% of the usual
height.

@var{mirrored?} indicates if the hairpin is mirrored over the y@tie{}axis or
if just the upper part is drawn.

Returns a function that accepts a hairpin grob as an argument
and draws the stencil based on its coordinates.

@c @lilypond is not allowed in the IR.
@example
#(define simple-hairpin
  (elbowed-hairpin '((0 . 0)(1.0 . 1.0)) #t))

\\relative c' @{
  \\override Hairpin #'stencil = #simple-hairpin
  a\\p\\< a a a\\f
@}
@end example
"
  (define (scale-coords coords-list x y)
    (map
     (lambda (coord) (cons (* x (car coord)) (* y (cdr coord))))
     coords-list))

  (define (hairpin::print-part points decresc? me)
    (let ((stil (make-connected-line points me)))
      (if decresc? (ly:stencil-scale stil -1 1) stil)))

  ;; outer let to trigger suicide
  (let ((sten (ly:hairpin::print grob)))
    (if (grob::is-live? grob)
        (let* ((decresc? (eqv? (ly:grob-property grob 'grow-direction) LEFT))
               (xex (ly:stencil-extent sten X))
               (lenx (interval-length xex))
               (yex (ly:stencil-extent sten Y))
               (leny (interval-length yex))
               (xtrans (+ (car xex) (if decresc? lenx 0)))
               (ytrans (car yex))
               (uplist (scale-coords coords lenx (/ leny 2)))
               (downlist (scale-coords coords lenx (/ leny -2)))
               (stil
                (ly:stencil-aligned-to
                 (ly:stencil-translate
                  (ly:stencil-add
                   (hairpin::print-part uplist decresc? grob)
                   (if mirrored?
                       (hairpin::print-part downlist decresc? grob)
                       empty-stencil))
                  (cons xtrans ytrans))
                 Y CENTER))
               (stil-y-extent (ly:stencil-extent stil Y)))
          ;; Return a final stencil properly aligned in Y-axis direction and with
          ;; proper extents. Otherwise stencil-operations like 'box-stencil' will
          ;; return badly. Extent in X-axis direction is taken from the original,
          ;; in Y-axis direction from the new stencil.
          (ly:make-stencil (ly:stencil-expr stil) xex stil-y-extent))
        ;; return empty, if no Hairpin.stencil present.
        '())))

(define-public flared-hairpin
  (elbowed-hairpin '((0 . 0) (0.95 . 0.4) (1.0 . 1.0)) #t))

(define-public constante-hairpin
  (elbowed-hairpin '((0 . 0) (1.0 . 0.0) (1.0 . 1.0)) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lyrics

(define-public (lyric-text::print grob)
  "Allow interpretation of tildes as lyric tieing marks."
  ;; See also similar code in Lyric_performer.
  (let ((text (ly:grob-property grob 'text)))

    (grob-interpret-markup grob (if (string? text)
                                    (make-tied-lyric-markup text)
                                    text))))

(define-public ((grob::calc-property-by-copy prop) grob)
  (ly:event-property (event-cause grob) prop))

(define-public (lyric-hyphen::vaticana-style grob)
  "Draw a @code{LyricHyphen} grob as needed for Gregorian chant in
Editio Vaticana style, that is, apply it once, flush-left.  If the
@code{text} property of @code{LyricHyphen} is set, print this markup.
If the property is not set, use a hyphen character."

  (define (span-point side common dir)
    (let ((iv (ly:grob-robust-relative-extent side common X)))
      (interval-bound iv dir)))

  (define (get-text-stencil grob)
    (grob-interpret-markup
     grob
     (ly:grob-property grob 'text "-")))

  (let* ((left-bound (ly:spanner-bound grob LEFT))
         (right-bound (ly:spanner-bound grob RIGHT))
         (common (ly:grob-common-refpoint left-bound right-bound X))
         (left-span (span-point left-bound common RIGHT))
         (right-span (span-point right-bound common LEFT))
         (span-length (- right-span left-span))
         (padding (ly:grob-property grob 'padding 0.1))
         (dash-sten (get-text-stencil grob))
         (dash-extent (ly:stencil-extent dash-sten X))
         (dash-length (interval-length dash-extent))
         (usable-length
          (- span-length
             (if (zero? (ly:item-break-dir left-bound)) padding 0)
             (if (zero? (ly:item-break-dir right-bound)) padding 0)))
         (offset
          (+ (- left-span (ly:grob-relative-coordinate grob common X))
             (if (zero? (ly:item-break-dir left-bound)) padding 0))))

    (if (or (< dash-length usable-length)
            (negative? (ly:item-break-dir right-bound)))
        (ly:stencil-translate-axis
         dash-sten offset X))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general inheritance

(define-public ((grob::inherit-parent-property axis property . default) grob)
  "@var{grob} callback generator for inheriting a @var{property} from
an @var{axis} parent, defaulting to @var{default} if there is no
parent or the parent has no setting."
  (let ((parent (ly:grob-parent grob axis)))
    (cond
     ((ly:grob? parent)
      (apply ly:grob-property parent property default))
     ((pair? default) (car default))
     (else '()))))

(define ((grob::relay-other-property property) grob)
  "@var{grob} callback generator for returning the value of another
property, which is identified by the symbol @var{property}."
  (ly:grob-property grob property))
(export grob::relay-other-property)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; balloons

;; TODO: What if the host of the broken piece picked here is dead?
;; For a hairpin, the last part is typically removed.  The problem
;; is that for grobs with more complex behavior, suicide could happen
;; late in the process.

;; TODO: Make interface for items more similar to spanner-placement
;; for spanners?

(define-public (ly:balloon-interface::remove-irrelevant-spanner grob)
  (if (ly:spanner? grob)
      (let* ((spanner-placement (ly:grob-property grob 'spanner-placement))
             (irrelevant?
              (cond
               ((eqv? spanner-placement LEFT)
                not-first-broken-spanner?)
               ((eqv? spanner-placement RIGHT)
                not-last-broken-spanner?)
               (else
                (ly:grob-warning
                 grob
                 'after-line-breaking
                 "spanner-placement must be #LEFT or #RIGHT, found ~s"
                 spanner-placement)
                not-first-broken-spanner?))))
        (if (irrelevant? grob)
            (ly:grob-suicide! grob)))))

(define-public balloon::height
  (ly:make-unpure-pure-container
   ly:grob::stencil-height
   ly:balloon-interface::pure-height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fret boards

(define-public (fret-board::calc-stencil grob)
  (grob-interpret-markup
   grob
   (make-fret-diagram-verbose-markup
    (ly:grob-property grob 'dot-placement-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slurs

(define-public slur::height
  (ly:make-unpure-pure-container
   ly:slur::height
   ly:slur::pure-height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scripts

(define-public (caesura-script-interface::before-line-breaking script)
  "Callback for @code{CaesuraScript} grob.  Eliminate scripts aligned to
bar lines if they might collide with a span bar.  Some types of bar
lines have visible span bars and some don't.  For consistent notation,
we don't check whether particular @code{SpanBar} grobs are actually
visible, just that they exist."
  (define (find-span-bars)
    (let ((parent (ly:grob-parent script X)))
      (if (grob::has-interface parent 'bar-line-interface)
          (ly:grob-object parent 'has-span-bar)
          #f)))

  (let ((span-bars (find-span-bars)))
    (when (pair? span-bars)
      (let* ((dir (ly:grob-property script 'direction))
             (span-bar (index-cell span-bars dir)))
        (when (ly:grob? span-bar)
          (ly:grob-suicide! script))))))

(define-public (script-interface::calc-x-offset grob)
  (ly:grob-property grob 'positioning-done)
  (let* ((shift-when-alone (ly:grob-property grob 'toward-stem-shift 0.0))
         (shift-in-column (ly:grob-property grob 'toward-stem-shift-in-column))
         (script-column (ly:grob-object grob 'script-column #f))
         (shift
          (if (and script-column
                   (number? shift-in-column)
                   ;; ScriptColumn can contain grobs other than Script.
                   ;; These should not result in a shift.
                   (any (lambda (s)
                          (and (not (eq? s grob))
                               (grob::has-interface s 'script-interface)
                               (not (grob::has-interface s
                                                         'accidental-suggestion-interface))))
                        (ly:grob-array->list
                         (ly:grob-object script-column 'scripts))))
              shift-in-column shift-when-alone))
         (note-head-location
          (ly:self-alignment-interface::aligned-on-x-parent grob))
         (note-head-grob (ly:grob-parent grob X))
         (stem-grob (ly:grob-object note-head-grob 'stem #f)))

    (+ note-head-location
       ;; If the script has the same direction as the stem, move the script
       ;; in accordance with the value of 'shift'.  Since scripts can also be
       ;; over skips, we need to check whether the grob has a stem at all.
       (if stem-grob
           (let ((dir1 (ly:grob-property grob 'direction))
                 (dir2 (ly:grob-property stem-grob 'direction)))
             (if (equal? dir1 dir2)
                 (let* ((common-refp (ly:grob-common-refpoint grob stem-grob X))
                        (stem-location
                         (ly:grob-relative-coordinate stem-grob common-refp X)))
                   (* shift (- stem-location note-head-location)))
                 0.0))
           0.0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instrument names

(define-public (system-start-text::print grob)
  (let* ((left-bound (ly:spanner-bound grob LEFT))
         (left-mom (ly:grob-property left-bound 'when))
         (name (if (moment<=? left-mom ZERO-MOMENT)
                   (ly:grob-property grob 'long-text)
                   (ly:grob-property grob 'text))))

    (if (and (markup? name)
             (!= (ly:item-break-dir left-bound) CENTER))

        (grob-interpret-markup grob name)
        (ly:grob-suicide! grob))))

(define-public (system-start-text::calc-x-offset grob)
  (let* ((left-bound (ly:spanner-bound grob LEFT))
         (left-mom (ly:grob-property left-bound 'when))
         (layout (ly:grob-layout grob))
         (indent (ly:output-def-lookup layout
                                       (if (moment<=? left-mom ZERO-MOMENT)
                                           'indent
                                           'short-indent)
                                       0.0))
         (system (ly:grob-system grob))
         (my-extent (ly:grob-extent grob system X))
         (elements (ly:grob-object system 'elements))
         (common (ly:grob-common-refpoint-of-array system elements X))
         (total-left +inf.0)
         (align-x (ly:grob-property grob 'self-alignment-X 0))
         (padding (min 0 (- (interval-length my-extent) indent)))
         (right-padding (- padding
                           (/ (* padding (1+ align-x)) 2))))

    ;; compensate for the variation in delimiter extents by
    ;; calculating an X-offset correction based on the extents
    ;; of all delimiters in this system
    ;; finally we take most-left coordinate and indent
    (let most-left-delim-x ((l (ly:grob-array-length elements)))
      (if (> l 0)
          (let ((elt (ly:grob-array-ref elements (1- l))))
            (if (grob::has-interface elt 'system-start-delimiter-interface)
                (let ((dims (ly:grob-extent elt common X)))
                  (set! total-left (min total-left (car dims)))))
            (most-left-delim-x (1- l)))))

    (+
     (ly:side-position-interface::x-aligned-side grob)
     right-padding
     (- (interval-length (cons total-left indent))))))

(define-public (system-start-text::calc-y-offset grob)

  (define (live-elements-list me)
    (let ((elements (ly:grob-object me 'elements)))

      (filter! grob::is-live?
               (ly:grob-array->list elements))))

  (let* ((left-bound (ly:spanner-bound grob LEFT))
         (live-elts (live-elements-list grob))
         (system (ly:grob-system grob))
         (extent empty-interval))

    (if (and (pair? live-elts)
             (interval-sane? (ly:grob-extent grob system Y)))
        (let get-extent ((lst live-elts))
          (if (pair? lst)
              (let ((axis-group (car lst)))

                (if (and (ly:spanner? axis-group)
                         (equal? (ly:spanner-bound axis-group LEFT)
                                 left-bound))
                    (set! extent (add-point extent
                                            (ly:grob-relative-coordinate
                                             axis-group system Y))))
                (get-extent (cdr lst)))))
        ;; no live axis group(s) for this instrument name -> remove from system
        (ly:grob-suicide! grob))

    (+
     (ly:self-alignment-interface::y-aligned-on-self grob)
     (interval-center extent))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; axis group interface

(define-public axis-group-interface::height
  (ly:make-unpure-pure-container
   ly:axis-group-interface::height
   ly:axis-group-interface::pure-height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ambitus

;; Calculate the gaps between ambitus heads and ends of ambitus line.
;; Start by determining desired length of the ambitus line (based on
;; length-fraction property), calc gap from that and make sure that
;; it doesn't exceed maximum allowed value.

(define-public (ambitus-line::calc-gap grob)
  (let ((heads (ly:grob-object grob 'note-heads #f)))

    (if (and heads
             (= (ly:grob-array-length heads) 2))
        (let* ((common (ly:grob-common-refpoint-of-array grob heads Y))
               (head-down (ly:grob-array-ref heads 0))
               (head-up (ly:grob-array-ref heads 1))
               (fraction (ly:grob-property grob 'length-fraction 0.7))
               (max-gap (ly:grob-property grob 'maximum-gap 0.45))
               ;; distance between noteheads:
               (distance (- (interval-start (ly:grob-extent head-up common Y))
                            (interval-end (ly:grob-extent head-down common Y))))
               (gap (* 0.5 distance (- 1 fraction))))

          (min gap max-gap))
        0)))

;; Print a line connecting ambitus heads:

(define-public (ambitus::print grob)
  (let ((heads (ly:grob-object grob 'note-heads #f)))

    (if (and heads
             (= (ly:grob-array-length heads) 2))
        (let* ((common (ly:grob-common-refpoint-of-array grob heads Y))
               (head-down (ly:grob-array-ref heads 0))
               (head-up (ly:grob-array-ref heads 1))
               ;; The value used when 'gap' property cannot be read is small
               ;; to make sure that ambitus of a fifth will have a visible line.
               (gap (ly:grob-property grob 'gap 0.25))
               (point-min (+ (interval-end (ly:grob-extent head-down common Y))
                             gap))
               (point-max (- (interval-start (ly:grob-extent head-up common Y))
                             gap)))

          (if (< (+ point-min 0.1) point-max) ; don't print lines shorter than 0.1ss
              (let* ((layout (ly:grob-layout grob))
                     (line-thick (ly:output-def-lookup layout 'line-thickness))
                     (blot (ly:output-def-lookup layout 'blot-diameter))
                     (grob-thick (ly:grob-property grob 'thickness 2))
                     (width (* line-thick grob-thick))
                     (x-ext (symmetric-interval (/ width 2)))
                     (y-ext (cons point-min point-max))
                     (line (ly:round-filled-box x-ext y-ext blot))
                     (y-coord (ly:grob-relative-coordinate grob common Y)))

                (ly:stencil-translate-axis line (- y-coord) Y))
              empty-stencil))
        (begin
          (ly:grob-suicide! grob)
          (list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  laissez-vibrer tie

(define-public (semi-tie::calc-cross-staff grob)
  (let* ((note-head (ly:grob-object grob 'note-head))
         (stem (ly:grob-object note-head 'stem #f)))
    (and stem
         (ly:grob-property stem 'cross-staff #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; volta-bracket

(define-public (volta-bracket-interface::pure-height grob start end)
  (let ((edge-height (ly:grob-property grob 'edge-height)))
    (if (number-pair? edge-height)
        (let ((smaller (min (car edge-height) (cdr edge-height)))
              (larger (max (car edge-height) (cdr edge-height))))
          (interval-union '(0 . 0) (cons smaller larger)))
        '(0 . 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; centered-spanner-interface

(define-public (centered-spanner-interface::calc-x-offset grob)
  "Compute the shift from this spanner's reference point to a point
centered between two non-musical columns, according to the
@code{spacing-@/pair} property.  This also takes
@code{self-@/alignment-X} into account.  The default for
@code{spacing-@/pair} is @code{'(break-@/alignment
.@tie{}break-@/alignment)}."
  (let* ((left-bound (ly:spanner-bound grob LEFT))
         (right-bound (ly:spanner-bound grob RIGHT))
         (refp (ly:grob-common-refpoint left-bound right-bound X))
         (base-position (ly:grob-relative-coordinate grob refp X))
         (spacing-pair
          (ly:grob-property grob
                            'spacing-pair
                            '(break-alignment . break-alignment)))
         (ext-L (ly:paper-column::break-align-width left-bound
                                                    (car spacing-pair)))
         (L-end (interval-end ext-L))
         (ext-R (ly:paper-column::break-align-width right-bound
                                                    (cdr spacing-pair)))
         (R-start (interval-start ext-R))
         ;; Amount of translation from our basic position to
         ;; the right of our left bound.
         (to-left-bound (- L-end base-position))
         ;; From that to the middle between left and right bound.
         (to-middle (* 0.5 (- R-start L-end)))
         ;; Correction to align according to self-alignment-X.
         (alignment-translation
          (ly:self-alignment-interface::x-aligned-on-self grob)))
    (+ to-left-bound to-middle alignment-translation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Measure counter.

(define-public (measure-counter::text grob)
  "A number for a measure count.  Broken measures are numbered in
parentheses.  When the counter spans several measures (like with
compressed multi-measure rests), it displays a measure range."
  (let* ((left-number-text (ly:grob-property grob 'left-number-text))
         (right-number-text (ly:grob-property grob 'right-number-text))
         (left-markup
          (if (markup? left-number-text) left-number-text empty-markup))
         (sep-and-right-markup
          (if (markup? right-number-text)
              (make-line-markup
               (list
                (let ((sep (ly:grob-property grob 'number-range-separator)))
                  (if (markup? sep) sep empty-markup))
                (if (markup? right-number-text) right-number-text empty-markup)))
              empty-markup))
         (text (make-line-markup (list left-markup sep-and-right-markup)))
         (orig (ly:grob-original grob))
         (siblings (ly:spanner-broken-into orig))) ; have we been split?
    (if (or (null? siblings)
            (eq? grob (car siblings)))
        text
        (make-parenthesize-markup text))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HorizontalBracketText

(define-public (ly:horizontal-bracket-text::print grob)
  (let ((text (ly:grob-property grob 'text)))
    (if (or (null? text)
            (equal? text "")
            (equal? text empty-markup))
        (begin
          (ly:grob-suicide! grob)
          '())
        (let* ((orig (ly:grob-original grob))
               (siblings (ly:spanner-broken-into orig))
               (text
                (if (or (null? siblings)
                        (eq? grob (car siblings)))
                    text
                    (if (string? text)
                        (string-append "(" text ")")
                        (make-parenthesize-markup text)))))
          (grob-interpret-markup grob text)))))

(define-public (ly:horizontal-bracket-text::calc-direction grob)
  (let* ((bracket (ly:grob-object grob 'bracket))
         (bracket-dir (ly:grob-property bracket 'direction DOWN)))
    bracket-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BendSpanner

(define (quarterdiff->string alist quarterdiff)
  "Takes the alist @var{alist}, stored in
@code{BendSpanner.details.bend-amount-strings}.  Applied to @var{quarterdiff},
which is supposed to be an integer representing the amount of quarter-steps
between two pitches, it returns a fraction as a string like ¾, probably a mixed
number string like 1½.
The actual formatting relies on the settings in @var{alist}."
  (let ((wholesteps (floor (/ quarterdiff 4))))
    (format #f "~a~a"
            (case wholesteps
              ((0) "")
              ((1) (if (and (zero? (modulo quarterdiff 4))
                            (assoc-get 'full alist))
                       (assoc-get 'full alist)
                       wholesteps))
              (else wholesteps))
            (case (modulo quarterdiff 4)
              ((1) (or (assoc-get 'quarter alist) "¼"))
              ((2) (or (assoc-get 'half alist) "½"))
              ((3) (or (assoc-get 'three-quarter alist)"¾"))
              (else "")))))

(define (get-quarter-diffs pitch-list-pair)
  "Takes @var{pitch-list-pair}, which is expected to be a pair of lists, both
containing pitches, and to be sorted with @code{ly:pitch<?} in advance.
Returns a number representing the amount of quarter tones between the highest
pitches from the two sub-lists."
  ;; For event-chords, bendings to different amounts are very unlikely, so we take
  ;; the highest pitch of every sublist for comparison.
  (let* ((starting-pitch-list (car pitch-list-pair))
         (target-pitch-list (cdr pitch-list-pair))
         (top-starting-pitch (last starting-pitch-list))
         (top-target-pitch (last target-pitch-list)))
    (- (ly:pitch-quartertones top-target-pitch)
       (ly:pitch-quartertones top-starting-pitch))))

(define-public (get-bound-note-heads spanner)
  "Take a spanner grob and return a pair containing all note heads of the
initial starting and the final @code{NoteColumn}."
  (let* ((orig (ly:grob-original spanner))
         ;; get the bend-spanner's starting/ending NoteColumns
         (left-bound (ly:spanner-bound orig LEFT))
         (right-bound (ly:spanner-bound orig RIGHT))
         ;; get their NoteHeads
         (left-note-heads-array (ly:grob-object left-bound 'note-heads #f))
         (right-note-heads-array (ly:grob-object right-bound 'note-heads #f)))
    (if (and left-note-heads-array right-note-heads-array)
        (cons
         (ly:grob-array->list left-note-heads-array)
         (ly:grob-array->list right-note-heads-array))
        ;; TODO display proper location
        ;;      sth at the lines of (*location*) ...
        (ly:error
         "~a needs NoteColumns with NoteHeads as bounds: ~a"
         spanner
         (list left-note-heads-array right-note-heads-array)))))

(define (bend::remove-certain-tab-note-heads tab-note-heads)
  "Takes the list @var{tab-note-heads} and removes all note heads, which
are played on open strings, unless grob-property @code{bend-me} is set to
@code{#t}.
Other note heads with @code{bend-me} set to @code{#f} are removed as well."
  (remove
   (lambda (tnh) (not (ly:grob-property tnh 'bend-me)))
   tab-note-heads))

(define (bounding-note-heads-pitches spanner)
  "Takes a spanner grob, gets the note heads from the starting and ending bound,
applying @code{get-bound-note-heads}, removes not spanned ones, gets the pitches
of the note heads of each bound, sorts them with @code{ly:pitch<?} and returns
them as a pair."
  (let* ((all-left-right-note-heads (get-bound-note-heads spanner))
         (left-notes
          (bend::remove-certain-tab-note-heads
           (car all-left-right-note-heads)))
         (right-notes
          (bend::remove-certain-tab-note-heads
           (cdr all-left-right-note-heads)))
         (left-pitches
          (map
           (lambda (note-head)
             (ly:event-property (event-cause note-head) 'pitch))
           left-notes))
         (sorted-left-pitches (sort left-pitches ly:pitch<?))
         (right-pitches
          (map
           (lambda (note-head)
             (ly:event-property (event-cause note-head) 'pitch))
           right-notes))
         (sorted-right-pitches (sort right-pitches ly:pitch<?)))
    (cons sorted-left-pitches sorted-right-pitches)))

(define-public (bend::target-cautionary spanner)
  "Set @code{'display-cautionary} of all relevant note heads of spanners right
bound to true.  As a result they appear parenthesized.
This procedure is the default value of @code{'before-line-breaking}."
  (let* ((all-right-note-heads (cdr (get-bound-note-heads spanner)))
         (right-note-heads
          (bend::remove-certain-tab-note-heads all-right-note-heads)))
    (for-each
     (lambda (right-tab-nh)
       (ly:grob-set-property! right-tab-nh 'display-cautionary #t))
     right-note-heads)))

(define-public (bend::text-string spanner)
  "Take a spanner grob and calculate a list with the quarter tone diffs between
the pitches of starting and ending bound.  Because bending to different amounts
is very unlikely, only the first element of this list is returned as a string."
  (let* ((sorted-left-right-pitches (bounding-note-heads-pitches spanner))
         (quarter-diffs (get-quarter-diffs sorted-left-right-pitches))
         (bend-amount-strings
          (assoc-get
           'bend-amount-strings
           (ly:grob-property spanner 'details)
           '())))
    (quarterdiff->string bend-amount-strings quarter-diffs)))

(define (get-top-most-tab-head tab-heads-list)
  "Get the @code{TabNoteHead} with the highest value of @code{'staff-position}
from @var{tab-heads-list}."
  ;; TODO this may cause wrong results for unusual string-tunings
  (car
   (sort
    tab-heads-list
    (lambda (tnh1 tnh2)
      (>
       (ly:grob-property tnh1 'staff-position)
       (ly:grob-property tnh2 'staff-position))))))

(define (make-tab-heads-transparent tab-heads)
  "Set @code{transparent} to @code{#t} in @code{TabVoice} for the bends target
note head.  If @code{\\tabFullNotation} is set, the stem and flag will be
transparent as well.
Doesn't work for beams."
  ;; TODO Needs design decision whether beams should be transparent or not.
  ;; If yes, a different approach is needed.
  (for-each
   (lambda (tab-note-head)
     (if (grob::has-interface tab-note-head 'tab-note-head-interface)
         (let* ((stem (ly:grob-object tab-note-head 'stem #f))
                (flag (ly:grob-object stem 'flag #f))
                (dot (ly:grob-object tab-note-head 'dot #f)))
           (if stem (ly:grob-set-property! stem 'transparent #t))
           (if flag (ly:grob-set-property! flag 'transparent #t))
           (if dot (ly:grob-set-property! dot 'transparent #t))
           (ly:grob-set-property! tab-note-head 'transparent #t))))
   tab-heads))

(define-public (bend::calc-bend-x-end
                bend-spanner top-left-tab-nhd top-right-tab-nhd)
  "Calculate the ending x@tie{}coordinate of @var{bend-spanner}.  At the line
end, take the items of @code{BreakAlignGroup} into account and a little
bit of padding.  Ends an unbroken spanner or the last of a broken one in the
middle of the topmost note head of its bounding note column."
  (let ((top-right-tab-nhd-x-ext
         (ly:grob-extent top-right-tab-nhd top-right-tab-nhd X))
        (curve-x-padding-line-end
         (assoc-get
          'curve-x-padding-line-end
          (ly:grob-property bend-spanner 'details)
          0))
        (sys (ly:grob-system bend-spanner))
        (right-bound
         (ly:spanner-bound bend-spanner RIGHT)))

    (if (middle-broken-spanner? bend-spanner)
        (ly:warning
         (G_ "~a with two line-breaks is not yet supported")
         bend-spanner))

    (if (unbroken-or-first-broken-spanner? bend-spanner)
        ;; For the first part of a broken bend-spanner ensure avoiding items of
        ;; BreakAlignGroup by taking its most left item's coordinate into the
        ;; calculation.
        (let* ((right-bound-elts
                (list-copy
                 (ly:grob-array->list
                  (ly:grob-object right-bound 'elements))))
               (right-break-align-grobs
                (if (grob::has-interface right-bound 'paper-column-interface)
                    (filter
                     (lambda (g)
                       (grob::has-interface g 'break-aligned-interface))
                     right-bound-elts)
                    (list right-bound)))
               (right-break-align-grobs-left-most
                (car
                 (sort
                  right-break-align-grobs
                  (lambda (g1 g2)
                    (<
                     (ly:grob-relative-coordinate g1 sys X)
                     (ly:grob-relative-coordinate g2 sys X))))))
               (right-ref-grob-coord
                (ly:grob-relative-coordinate
                 right-break-align-grobs-left-most sys X))
               (left-bound
                (ly:spanner-bound bend-spanner LEFT))
               (left-bound-x-coord
                (if (ly:grob? left-bound)
                    (ly:grob-relative-coordinate
                     left-bound
                     (ly:grob-common-refpoint left-bound sys X)
                     X)
                    0)))
          (+
           (-
            right-ref-grob-coord
            left-bound-x-coord
            (if (unbroken-spanner? bend-spanner) 0 curve-x-padding-line-end))
           ;; ensure it ends in the middle of the topmost TabNoteHead of the
           ;; right-bounding NoteColumn, if any.
           (if (unbroken-spanner? bend-spanner)
               (interval-center top-right-tab-nhd-x-ext)
               0)))
        ;; last of broken bend-spanner
        ;; simply take the right-bound coordinate
        (let* ((right-bound-x-coord
                (if (ly:grob? right-bound)
                    (ly:grob-relative-coordinate right-bound sys X)
                    0)))
          (+ right-bound-x-coord (interval-center top-right-tab-nhd-x-ext))))))

(define-public (bend::calc-bend-x-begin
                bend-spanner bounding-noteheads factor quarter-tone-diffs)
  "Calculate the starting values in x@tie{}direction of the bend.
After a line break, the values from the right bound are taken minus
1.5@tie{}staff spaces.
For bends-down or if grob property @code{'style} equals to @code{'pre-bend},
@code{'hold} or @code{'pre-bend-hold}, @code{interval-center} is applied the
topmost note head of the starting note heads.
In any other case the right edge of the starting note head is used.  The value
of @code{BendSpanner.@/details.@/horizontal-left-padding} is added, which may be
changed by an appropriate override.
Returns a list of the same length as the amount of bend-starting note heads."
  (let* ((staff-space (ly:staff-symbol-staff-space bend-spanner))
         (left-note-heads-list-length (length (car bounding-noteheads)))
         (style (ly:grob-property bend-spanner 'style))
         (horizontal-left-padding
          (assoc-get
           'horizontal-left-padding
           (ly:grob-property bend-spanner 'details 0)))
         (top-left-tab-nhd (get-top-most-tab-head (car bounding-noteheads)))
         (top-right-tab-nhd (get-top-most-tab-head (cdr bounding-noteheads)))
         (top-left-tab-nhd-x-ext
          (ly:grob-extent top-left-tab-nhd top-left-tab-nhd X)))
    (cond ((end-broken-spanner? bend-spanner)
           (make-list
            left-note-heads-list-length
            (- (bend::calc-bend-x-end
                bend-spanner top-left-tab-nhd top-right-tab-nhd)
               ;; 1.5 is my choice, harm
               (* 1.5 staff-space))))
          ((or (negative? quarter-tone-diffs)
               (eq? style 'pre-bend)
               (eq? style 'pre-bend-hold)
               (eq? style 'hold)
               (> factor 1))
           (make-list
            left-note-heads-list-length
            (interval-center top-left-tab-nhd-x-ext)))
          (else
           (map
            (lambda (tnh)
              (+ (cdr (ly:grob-extent tnh tnh X)) horizontal-left-padding))
            (car bounding-noteheads))))))

(define* (bend::calc-y-coordinates
          bend-spanner staff-space tab-note-heads
          #:optional quarter-tones-diffs)
  "Calculates Y-coordinate of @var{bend-spanner}'s start/end in relation to the
provided @var{tab-note-heads}.
For style @code{'pre-bend} or @code{'pre-bend-hold} or if the bend does not
point up the resulting value is offset to the top of the topmost note head.
Some vertical padding is added, taken from @var{bend-spanner}'s @code{'details}
sub-property @code{'vertical-padding}."
  (let* ((vertical-padding
          (assoc-get
           'vertical-padding
           (ly:grob-property bend-spanner 'details)))
         (style (ly:grob-property bend-spanner 'style))
         (quarter-tones (if quarter-tones-diffs (list quarter-tones-diffs) '()))
         (top-tab-note-head (get-top-most-tab-head tab-note-heads))
         (relevant-tab-note-heads
          (if (or (eq? style 'pre-bend)
                  (eq? style 'pre-bend-hold)
                  (and quarter-tones-diffs (negative? quarter-tones-diffs))
                  (not quarter-tones-diffs))
              (list top-tab-note-head)
              tab-note-heads)))
    (sort
     (map
      (lambda (tab-nh)
        (+
         (* (/ (ly:grob-property tab-nh 'staff-position) 2) staff-space)
         (if (or (eq? style 'pre-bend)
                 (eq? style 'pre-bend-hold)
                 (and quarter-tones-diffs (negative? quarter-tones-diffs))
                 (not quarter-tones-diffs))
             (+
              (cdr (ly:grob-extent top-tab-note-head top-tab-note-head Y))
              vertical-padding)
             (* vertical-padding staff-space))))
      relevant-tab-note-heads)
     <)))

;; The final BendSpanner.stencil is calculated in several steps:
;;  - the text stencil
;;  - the arrow head stencil
;;  - the lines and curves
;;    - a basic printing procedure
;;    - joining the results of the basic printing procedure into one stencil
;;  - the final stencil putting together text, arrow head, lines and curves
(define (bend::text-stencil x y text grob)
  "Returns a stencil that prints the bends amount, translated to the end of
the bends arrow head, given by @var{x} and@tie{}@var{y}.  A little vertical
padding is added."
  (let* ((layout (ly:grob-layout grob))
         (props (ly:grob-alist-chain grob))
         (staff-space (ly:staff-symbol-staff-space grob))
         (font-size (ly:grob-property grob 'font-size 0.0))
         (scale-factor (magstep font-size))
         (vertical-padding
          (assoc-get
           'vertical-padding
           (ly:grob-property grob 'details))))
    (ly:stencil-translate
     (ly:stencil-aligned-to
      (interpret-markup layout props text)
      X CENTER)
     ;; double vertical-padding is my choice, harm
     ;; TODO let it rely on a separate details-property?
     (cons x (+ y (* scale-factor vertical-padding 2))))))

(define-public (bend::arrow-head-stencil
                thickness x-y-coords height width dir)
  "Return an arrow head stencil, calculated from the given dimensions
@var{height} and @var{width}, and translated to @var{x-y-coords}, the end of
the bend-spanners (curved) line."
  (let* ((pts-list
          (list
           ;; horizontal-left point, x and y coord
           (/ width -2) 0
           ;; horizontal-right point, x and y coord
           (/ width 2) 0
           ;; arrow-top point, x and y coord
           0 (* height dir))))
    (ly:stencil-translate
     (ly:make-stencil
      `(polygon ,pts-list ,thickness #t)
      (interval-widen (cons (/ width -2) (/ width 2)) (/ thickness 2))
      (interval-widen (ordered-cons 0 (* height dir)) (/ thickness 2)))
     x-y-coords)))

(define* (bend::make-line-curve-stencil
          thickness points-list #:optional line-style)
  "Prints a possibly curved line.  If the line is horizontal a dashed line is
returned, relying on @var{line-style}.  By default this is
@code{BendSpanner.details.dashed-line-settings}."
  (let* ((pts-length (length points-list))
         (command-list
          (case pts-length
            ((4)
             `(moveto
               ,(list-ref points-list 0)
               ,(list-ref points-list 1)
               lineto
               ,(list-ref points-list 2)
               ,(list-ref points-list 3)))
            ((10)
             `(moveto
               ,(list-ref points-list 0)
               ,(list-ref points-list 1)
               lineto
               ,(list-ref points-list 2)
               ,(list-ref points-list 3)
               curveto
               ,(list-ref points-list 4)
               ,(list-ref points-list 5)
               ,(list-ref points-list 6)
               ,(list-ref points-list 7)
               ,(list-ref points-list 8)
               ,(list-ref points-list 9)))
            (else
             (ly:error
              "list-length ~a needs to have 4 or 10 elements" points-list)))))
    ;; For a horizontal line and proper settings of line-style return a
    ;; translated dashed-line-stencil.
    (if (and (= (list-ref points-list 1) (list-ref points-list 3))
             (= 4 pts-length)
             (= 3 (length+ line-style)))
        (let ((x-end (- (list-ref points-list 2) (car points-list))))
          (ly:stencil-translate
           (ly:make-stencil
            (list
             'dashed-line
             thickness
             (car line-style) ;; on
             (cadr line-style) ; off
             x-end ;; x-end
             0 ;; y-end
             (last line-style)) ;; phase
            ;; x-ext
            (cons 0 x-end)
            ;; y-ext
            (cons (/ thickness -2) (/ thickness 2)))
           (cons (car points-list) (cadr points-list))))
        (make-path-stencil command-list thickness 1 1 #f))))

(define (bend::draw-curves thickness begin-x middle-x end-x begin-y end-y grob)
  "Returns the combined stencils created by @code{bend::curve-stencils-list}.
All stencils let room to place an arrow head between them and the text
indicating the amount string is bent.
The descending y@tie{}value of the end point of the curves is taken from
@code{details.curve-y-padding-line-end}."
  ;; 'begin-x' will be provided by 'bend::calc-bend-x-begin', always
  ;; returning a list.
  ;; 'begin-y' is supposed to be a list, this should be ensured while calling
  ;; 'bend::draw-curves'
  (let* ((font-size (ly:grob-property grob 'font-size 0.0))
         (scale-factor (magstep font-size))
         (details (ly:grob-property grob 'details))
         (bend-dir
          (if (> end-y (apply max begin-y)) -1 1))
         (bend-arrowhead-height
          (* scale-factor (assoc-get 'bend-arrowhead-height details)))
         (curve-line-end-y-padding
          (if (not-last-broken-spanner? grob)
              (assoc-get 'curve-y-padding-line-end details 0)
              0))
         ;; recalculate `end-y' to make room for the arrow head
         (new-end-y
          (make-list
           (length begin-y)
           (+ end-y (* bend-arrowhead-height bend-dir))))
         (y-endings
          (if (zero? curve-line-end-y-padding) ;; i.e. no line-break
              new-end-y
              ;; otherwise return a list with descending values
              (reverse
               (map
                (lambda (y-end i) (- y-end (* i curve-line-end-y-padding)))
                new-end-y
                (iota (length begin-y))))))
         (curve-stils
          (map
           (lambda (beg-x beg-y ending-y)
             (let* ((lst
                     (list
                      ;; moveto
                      beg-x beg-y
                      ;; lineto
                      middle-x beg-y
                      ;; curveto
                      middle-x beg-y end-x beg-y end-x ending-y)))
               (bend::make-line-curve-stencil thickness lst)))
           begin-x
           begin-y
           y-endings)))
    (apply ly:stencil-add curve-stils)))

(define-public (bend-spanner::print grob)
  "Return the final stencil.  A line and curve, an arrow head and a text
representing the amount a string is bent."
  (let* (;; We need to get _all_ bounding tab-note-heads to calculate the
         ;; correct vertical position of the end of a down-spanner not only the
         ;; ones which actually starts a bend.
         ;; This is important, if the topmost string is not bent.
         ;; But for creating the bend-stencil(s) we only need those which are
         ;; actually bent.
         ;; If after selecting no note-heads remain, print a warning and suicide
         ;; the BendSpanner
         (details (ly:grob-property grob 'details))
         (all-left-right-note-heads (get-bound-note-heads grob))
         (factor (assoc-get 'successive-level details))
         (previous-dir (assoc-get 'previous-dir details))
         (left-right-note-heads
          (cons
           (bend::remove-certain-tab-note-heads
            (car all-left-right-note-heads))
           (bend::remove-certain-tab-note-heads
            (cdr all-left-right-note-heads)))))
    (cond
     ;; fool-proof, the user may have set a non-integer by accident
     ((not (integer? factor))
      (begin
        (ly:warning (G_ "Factor ~a needs to be an integer value." factor))
        (ly:grob-suicide! grob)))
     (else
      (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
             (line-count (ly:grob-property staff-symbol 'line-count))
             (staff-space (ly:staff-symbol-staff-space grob))
             (staff-symbol-line-thickness
              (ly:staff-symbol-line-thickness grob))
             ;; style may be '(), 'hold, 'pre-bend or 'pre-bend-hold
             (style (ly:grob-property grob 'style))
             (details (ly:grob-property grob 'details))
             (dashed-line-settings (assoc-get 'dashed-line-settings details))
             (font-size (ly:grob-property grob 'font-size 0.0))
             (scale-factor (magstep font-size))
             ;; get the topmost TabNoteHeads
             (top-right-tab-nh
              (get-top-most-tab-head (cdr all-left-right-note-heads)))
             (top-left-tab-nh
              (get-top-most-tab-head (car all-left-right-note-heads)))
             (sorted-left-right-pitches
              (bounding-note-heads-pitches grob))
             (quarter-diffs
              (get-quarter-diffs sorted-left-right-pitches))
             (bend-direction
              (if (negative? quarter-diffs) DOWN UP))
             (bend-up? (positive? bend-direction))
             (bend-down? (negative? bend-direction))
             (begin-x-list
              (bend::calc-bend-x-begin
               grob
               left-right-note-heads
               factor
               quarter-diffs))
             (begin-x (car begin-x-list))
             (begin-y-list
              (bend::calc-y-coordinates
               grob staff-space
               ;; for pre-bend and pre-bend-hold we take
               ;; all note-heads into account to avoid collisions
               ;; with a probably not bent open string
               (if (or (eq? style 'pre-bend)
                       (eq? style 'pre-bend-hold))
                   (car all-left-right-note-heads)
                   (car left-right-note-heads))
               quarter-diffs))
             (curve-x-padding-line-end
              (if (and (first-broken-spanner? grob)
                       (not (eq? style 'pre-bend-hold)))
                  (assoc-get 'curve-x-padding-line-end details 0)
                  0))
             (end-x
              (bend::calc-bend-x-end
               grob top-left-tab-nh top-right-tab-nh))
             (y-distance-from-tabstaff-to-arrow-tip
              (* scale-factor
                 (assoc-get 'y-distance-from-tabstaff-to-arrow-tip details)))
             (end-y
              (lambda (mult)
                (+
                 (* (/ (1- line-count) 2) staff-space)
                 (* mult y-distance-from-tabstaff-to-arrow-tip))))
             (bend-arrowhead-width
              (* scale-factor
                 (assoc-get 'bend-arrowhead-width details)))
             (bend-arrowhead-height
              (* scale-factor
                 (assoc-get 'bend-arrowhead-height details)))
             (bend-line-thickness
              (* staff-symbol-line-thickness
                 (ly:grob-property grob 'thickness)))
             ;; curvature-factor, usually between 0 and 1,
             ;; determines the horizontal part of a bend-spanner as percentage
             ;; of the total horizontal extent
             ;; 0.35 as fall-back is my choice, harm
             (curvature-factor
              (assoc-get 'curvature-factor details 0.35))
             (middle-x
              (+
               begin-x
               (* curvature-factor
                  (- end-x begin-x))
               ;; if the curve gets some padding at line-break
               ;; do it here as well - warrants nicer output
               (- curve-x-padding-line-end)))
             (arrow-stencil-proc (assoc-get 'arrow-stencil details))
             (target-visibility (assoc-get 'target-visibility details #f))
             ;; A vector of 3 booleans, #(end-of-line unbroken begin-of-line)
             (head-text-break-visibility
              (assoc-get 'head-text-break-visibility details))
             (head-text-print-condition
              (cond ((first-broken-spanner? grob)
                     (vector-ref head-text-break-visibility 0))
                    ((end-broken-spanner? grob)
                     (vector-ref head-text-break-visibility 2))
                    (else
                     (vector-ref head-text-break-visibility 1))))
             (bend-amount
              ;; Use 'text if set, fall back to 'bend::text-string'
              (or (ly:grob-property grob 'text #f)
                  (bend::text-string grob))))

        ;; For up-bends, make target note heads transparent.
        ;; If details.target-visibility is set #t they will be parenthesized
        ;;
        ;; Down-bends will get their target note-heads parenthesized via
        ;; 'display-cautionary.
        ;;
        ;; For tied notes all notes except the ones from the first
        ;; note-column become transparent
        (if (and (positive? quarter-diffs) (not target-visibility))
            (make-tab-heads-transparent
             (ly:grob-array->list
              (ly:grob-object
               (ly:spanner-bound (ly:grob-original grob) RIGHT)
               'note-heads))))

        ;; the final stencil
        (ly:stencil-add
         point-stencil
         ;; The text-stencil, indicating bend-amount
         ;;   printed for up-bends only
         ;;   in case of line-break it will be printed only at line-begin,
         ;;   for 'pre-bend and 'prebend-hold, it is printed at line end
         (if (and (> bend-direction -1) head-text-print-condition)
             (let ((text-pos
                    (if (or (eq? style 'pre-bend) (eq? style 'pre-bend-hold))
                        begin-x
                        end-x)))
               (bend::text-stencil
                text-pos
                (end-y factor)
                bend-amount
                grob))
             empty-stencil)

         (cond
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; hold
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ((eq? style 'hold)
           (let ((line-end-x-padding
                  (if (first-broken-spanner? grob)
                      (assoc-get 'curve-x-padding-line-end details 0)
                      0)))
             (bend::make-line-curve-stencil
              bend-line-thickness
              (list
               begin-x
               (end-y factor)
               (- end-x line-end-x-padding)
               (end-y factor))
              dashed-line-settings)))
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; pre-bend and pre-bend-hold
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ((or (eq? style 'pre-bend) (eq? style 'pre-bend-hold))
           (let* ((vertical-line
                   (if (unbroken-or-first-broken-spanner? grob)
                       (bend::make-line-curve-stencil
                        bend-line-thickness
                        (list
                         begin-x
                         (last begin-y-list)
                         begin-x
                         (end-y factor)))
                       empty-stencil))
                  (horizontal-line
                   (if (eq? style 'pre-bend-hold)
                       (let ((line-end-x-padding
                              (if (first-broken-spanner? grob)
                                  (assoc-get
                                   'curve-x-padding-line-end details 0)
                                  0)))
                         (bend::make-line-curve-stencil
                          bend-line-thickness
                          (list
                           begin-x
                           (end-y factor)
                           (- end-x line-end-x-padding)
                           (end-y factor))
                          dashed-line-settings))
                       empty-stencil))
                  (arrow-head
                   (if head-text-print-condition
                       (arrow-stencil-proc
                        bend-line-thickness
                        ;;end-curve-coords:
                        (cons
                         begin-x
                         (- (end-y factor)
                            bend-arrowhead-height))
                        bend-arrowhead-height
                        bend-arrowhead-width
                        bend-direction)
                       empty-stencil))
                  )
             (ly:stencil-add
              arrow-head
              vertical-line
              horizontal-line)))
              ;;;;;;;;;;;;;;;;;;;;;;
          ;; consecutive bends
              ;;;;;;;;;;;;;;;;;;;;;;
          (else
           (if (> factor 1)
               (let* ((val (if bend-down?
                               bend-direction
                               0))
                      (arrow-head
                       (if head-text-print-condition
                           (arrow-stencil-proc
                            bend-line-thickness
                            ;; end-curve-coords
                            (cons end-x
                                  (+ (end-y (+ factor val))
                                     (* -1
                                        bend-direction
                                        bend-arrowhead-height)))
                            bend-arrowhead-height
                            bend-arrowhead-width
                            bend-direction)
                           empty-stencil))
                      (curves
                       (bend::draw-curves
                        bend-line-thickness
                        begin-x-list middle-x
                        end-x
                        (list
                         (end-y
                          (+ factor val
                             (* -1 bend-direction))))
                        (end-y (+ factor val))
                        grob)))
                 (ly:stencil-add arrow-head curves))
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;;;; default bends, i.e. factor is 1
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
               (let* ((new-end-y
                       (if bend-up?
                           (end-y factor)
                           (let* ((end-y-list
                                   (bend::calc-y-coordinates
                                    grob
                                    staff-space
                                    (cdr all-left-right-note-heads))))
                             (last end-y-list))))
                      (new-begin-y
                       (if bend-up?
                           begin-y-list
                           (list (end-y factor))))
                      (arrow-head
                       (if head-text-print-condition
                           (arrow-stencil-proc
                            bend-line-thickness
                            ;; end-curve-coords
                            (cons
                             end-x
                             (+ new-end-y
                                (* -1
                                   bend-direction
                                   bend-arrowhead-height)))
                            bend-arrowhead-height
                            bend-arrowhead-width
                            bend-direction)
                           empty-stencil))
                      (curve
                       (bend::draw-curves
                        bend-line-thickness
                        begin-x-list
                        middle-x
                        end-x
                        new-begin-y
                        new-end-y
                        grob)))
                 (ly:stencil-add arrow-head curve)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DurationLine

;;;; We separate coding the stencil of DurationLine into
;;;; - coding arrow-stencil
;;;; - coding hook-stencil
;;;; - coding duration-line::calc (providing all needed values)
;;;; - coding duration-line::print (putting it all together)
;;;;
;;;; This lowers the hurdle to change those stencil-definitions for the user, if
;;;; he wants to.
(define-public (arrow-stencil x y thick staff-space grob)
  "Return a right-pointing, filled arrow-head, where @var{x} determines the basic
horizontal position and @var{y} determines the basic vertical position.
Both values are adjusted using @var{staff-space}, which is @code{StaffSymbol}'s
staff space.  @var{thick} is the used line thickness."
  (let* ((arrow-length (ly:grob-property grob 'arrow-length))
         (arrow-width (ly:grob-property grob 'arrow-width))
         (moveto-x
          ;; In duration-line::calc the right end is shortened a bit.
          ;; Thus place the arrow-tip to the right of it.
          ;; (* arrow-length (* staff-space 2/3)) is my choice for it, here and
          ;; in duration-line::calc, harm
          (+ x (* arrow-length (* staff-space 2/3))))
         (moveto-y (* y staff-space))
         ;; scale arrow-width/length by staff-space
         (scaled-arrow-width
          (* arrow-width staff-space))
         (scaled-arrow-length
          (* arrow-length staff-space)))
    (make-path-stencil
     (list
      'moveto moveto-x moveto-y
      'rlineto (- scaled-arrow-length) (/ scaled-arrow-width 2)
      'rlineto 0 (* scaled-arrow-width -1)
      'closepath)
     thick
     1 1 #t)))

(define-public (hook-stencil x y staff-space thick blot grob)
  "Return a hook stencil where @var{x} determines the horizontal position and
@var{y}@tie{}determines the basic vertical position.
The final stencil is adjusted vertically using @var{staff-space}, which is
@code{StaffSymbol}'s  staff space, and uses @var{blot}, which is the current
@code{'blot-diameter}.  The stencil's thickness is usually taken from @var{grob}
@code{'details}, @var{thick} serves as a fallback value."
  (let* ((details (ly:grob-property grob 'details))
         (hook-dir (assoc-get 'hook-direction details 1))
         (hook-height
          (assoc-get 'hook-height details (* staff-space 2/3)))
         (hook-thick
          (or (assoc-get 'hook-thickness details) thick))
         (hook-X-ext
          (cons (- x hook-thick) x))
         (hook-Y-ext
          (ordered-cons
           (* hook-dir (+ hook-height thick))
           0)))
    (ly:round-filled-box
     hook-X-ext
     (coord-translate hook-Y-ext (* y staff-space))
     blot)))

(define-public (duration-line::calc grob)
  "Return list of values needed to print a stencil for @code{DurationLine}."
  (let* ((note-head-or-rest?
          (lambda (x)
            (and (ly:grob? x)
                 (or (grob::has-interface x 'note-head-interface)
                     (grob::has-interface x 'rest-interface)))))
         (staff-space (ly:staff-symbol-staff-space grob))
         (grob-layout (ly:grob-layout grob))
         (layout-thick (layout-line-thickness grob))
         (blot-diameter
          (ly:output-def-lookup grob-layout 'blot-diameter))
         (style (ly:grob-property grob 'style 'beam))
         (details (ly:grob-property grob 'details))
         (thickness (ly:grob-property grob 'thickness 4))
         (left-bound-details
          (ly:grob-property grob 'left-bound-info))
         (right-bound-details
          (ly:grob-property grob 'right-bound-info))
         (start-at-dot?
          (assoc-get 'start-at-dot left-bound-details #f))
         (end-on-accidental?
          (assoc-get 'end-on-accidental right-bound-details))
         (end-on-arpeggio?
          (assoc-get 'end-on-arpeggio right-bound-details))
         (left-padding
          (assoc-get 'padding left-bound-details 0))
         (right-padding
          (assoc-get 'padding right-bound-details 0))
         (right-end-style
          (assoc-get 'end-style right-bound-details #f))
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;; DurationLine start
    ;;;;;;;;;;;;;;;;;;;;;;;;
         (left-bound (ly:spanner-bound grob LEFT))
         (left-column
          (if (note-head-or-rest? left-bound)
              (ly:grob-parent left-bound X)
              left-bound))
     ;;;;
     ;;;; adjust for DotColumn of left NoteColumn
     ;;;;
         ;; If DotColumn is present and `start-at-dot' is enabled, we want a
         ;; little extra padding, taken from details.extra-dot-padding.
         (dot-column (ly:note-column-dot-column left-column))
         (adjust-for-dot-column
          (if (and start-at-dot? (ly:grob? dot-column))
              (assoc-get 'extra-dot-padding details)
              0))
         ;; `left-X' is line-starting X-coordinate relative to grob's system
         ;; NB the final line-stencil will start at left-bound not at `left-X'
         ;;    we need this value to calculate `right-end' lateron
         (left-X
          (ly:grob-relative-coordinate
           left-bound (ly:grob-system left-bound) X))
         (left-info-X (assoc-get 'X left-bound-details))
         ;; `left-Y' is line-starting Y-coordinate, taken from staff-postion
         ;; of grob's first initiating NoteHead.
         (left-bound-original (ly:spanner-bound (ly:grob-original grob) LEFT))
         (raw-left-Y
          (if (grob::has-interface left-bound-original 'note-column-interface)
              (let* ((nhds-array
                      (ly:grob-object left-bound-original 'note-heads #f))
                     (nhds-list
                      (if nhds-array
                          (ly:grob-array->list nhds-array)
                          '()))
                     (nhds-pos
                      (map
                       (lambda (nhd) (ly:grob-property nhd 'staff-position))
                       nhds-list)))
                ;; the middle-y of NoteColumn
                (/ (+ (apply min nhds-pos) (apply max nhds-pos)) 2))
              ;; staff-position of NoteHead/Rest
              (ly:grob-property left-bound-original 'staff-position 0)))
         (left-Y
          (/ (+ (ly:grob-property grob 'Y-offset 0) raw-left-Y) 2))
    ;;;;
    ;;;; final calculation of `left-start'
    ;;;;

         ;; `left-start' is line-starting X-coordinate relative to left-bound
         ;;
         ;; If DurationLine is started at "skip", `left-bound' may be
         ;; PaperColumn, with non-zero x-extent. Their x-extent should be
         ;; disregarded in this case, using zero instead.
         (left-start
          (+ (if (and (unbroken-or-first-broken-spanner? grob)
                      (grob::has-interface left-bound 'paper-column-interface))
                 0
                 (- left-info-X left-X))
             adjust-for-dot-column
             left-padding))
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;; DurationLine end
    ;;;;;;;;;;;;;;;;;;;;;;;;
         (right-bound (ly:spanner-bound grob RIGHT))
    ;;;;
    ;;;; adjust or Arpeggio of right NoteColumn
    ;;;;
    ;;;; TODO: build this into the line-spanner-interface, allowing it
    ;;;; to be used on other line spanners.
         (arpeggio-start
          (if end-on-arpeggio?
              (let* ((conditional-elements
                      (ly:grob-object right-bound 'conditional-elements #f))
                     (cond-elts-list
                      (if conditional-elements
                          (ly:grob-array->list conditional-elements)
                          '()))
                     (arpeggio-ls
                      (filter
                       (lambda (g)
                         (grob::has-interface g 'arpeggio-interface))
                       cond-elts-list)))
                (if (and (pair? arpeggio-ls) (ly:grob? (car arpeggio-ls)))
                    (interval-start (ly:grob-extent (car arpeggio-ls)
                                                    (ly:grob-system grob)
                                                    X))
                    #f))
              #f))

    ;;;;
    ;;;; adjust for arrow
    ;;;;
         (adjust-for-arrow
          (if (eq? right-end-style 'arrow)
              ;; We do not go for the full arrow-length, to avoid a
              ;; visible gap for certain styles
              (* (ly:grob-property grob 'arrow-length)
                 (* staff-space 2/3))
              0))
    ;;;;
    ;;;; final calculation of `right-end'
    ;;;;
         (right-info-X
          (assoc-get 'X right-bound-details 0))
         ;; Repect padding and other possible items.
         (right-end
          (- (or arpeggio-start right-info-X)
             left-X
             right-padding
             adjust-for-arrow))

         ;; TODO find a method to accept user-generated line-ending stencils

    ;;;;
    ;;;; arrow
    ;;;;
         (arrow-stil
          (if (and (not (eq? style 'none))
                   (eq? right-end-style 'arrow))
              (begin
                ;; For 1/3 see remark in `arrow-stencil' above
                (if (> (* 1/3  staff-space (ly:grob-property grob 'arrow-length))
                       (- right-end left-start))
                    (ly:warning
                     (G_ "Not enough space to print a nice arrow.
Please consider to increase 'minimum-length or decrease 'arrow-length.")))
                (arrow-stencil
                 right-end left-Y layout-thick staff-space grob))
              empty-stencil))
    ;;;;;;;;
    ;;;; hook
    ;;;;;;;;
         (hook-stil
          ;; hooks are currently implemented for beam-style only
          (if (and (eq? style 'beam) (eq? right-end-style 'hook))
              (hook-stencil
               right-end left-Y staff-space thickness blot-diameter grob)
              empty-stencil)))

    (if (> left-start right-end)
        (ly:warning (G_ "Please consider to increase 'minimum-length")))

    ;;;;;;;;;;;;;;;;;;;;
    ;;;; final alist
    ;;;;;;;;;;;;;;;;;;;;

    (list
     (cons 'x-start left-start)
     (cons 'x-end right-end)
     (cons 'y left-Y)
     (cons 'staff-space staff-space)
     (cons 'blot blot-diameter)
     (cons 'style style)
     (cons 'thick thickness)
     (cons 'arrow arrow-stil)
     (cons 'hook hook-stil))))

(define-public (duration-line::print grob)
  "Return the stencil of @code{DurationLine}."
  (let* ((vals (duration-line::calc grob))
         (style (assoc-get 'style vals))
         (left-start (assoc-get 'x-start vals))
         (right-end (assoc-get 'x-end vals))
         (left-Y (assoc-get 'y vals))
         (staff-space (assoc-get 'staff-space vals))
         (blot-diameter (assoc-get 'blot vals))
         (thick (assoc-get 'thick vals))
         (hook-stil (assoc-get 'hook vals))
         (arrow-stil (assoc-get 'arrow vals)))

    (if (eq? style 'beam)
        (ly:stencil-add
         (ly:round-filled-box
          (cons left-start right-end)
          (coord-translate
           (cons (/ thick -2) (/ thick 2))
           (* left-Y staff-space))
          blot-diameter)
         hook-stil
         arrow-stil)
        (ly:stencil-add
         (ly:line-interface::line
          grob
          left-start
          (* left-Y staff-space)
          right-end
          (* left-Y staff-space))
         arrow-stil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finger glide spanner

(define-public finger-glide::print
  (lambda (grob)
    "The stencil printing procedure for grob @code{FingerGlideSpanner}.
Depending on the grob property @code{style} several forms of appearance are
printed.
Possible settings for grob property @code{style} are @code{zigzag},
@code{trill}, @code{dashed-line}, @code{dotted-line}, @code{stub-left},
@code{stub-right}, @code{stub-both}, @code{bow}, @code{none} and @code{line},
which is the default."
    (let* ((style (ly:grob-property grob 'style 'line)))
      (if (eq? style 'line)
          (ly:line-spanner::print grob)
          (let* ((thick (ly:grob-property grob 'thickness 1))
                 (line-thick (ly:staff-symbol-line-thickness grob))
                 (left-bound-info
                  (ly:grob-property grob 'left-bound-info))
                 (right-bound-info
                  (ly:grob-property grob 'right-bound-info))
                 (left-padding (assoc-get 'padding left-bound-info 0.5))
                 (right-padding (assoc-get 'padding right-bound-info 0.5))
                 (left-bound (ly:spanner-bound grob LEFT))
                 (right-bound (ly:spanner-bound grob RIGHT))
                 (sys (ly:grob-system grob))
                 (left-coord
                  (ly:grob-relative-coordinate left-bound sys X))
                 (X-left (assoc-get 'X left-bound-info))
                 (x-start (- X-left left-coord (- left-padding)))
                 (X-right (assoc-get 'X right-bound-info))
                 (x-end (- X-right left-coord (+ right-padding)))
                 (y-end (assoc-get 'Y right-bound-info))
                 (y-start
                  (cond (;; TODO sufficient?
                         (and (end-broken-spanner? grob) (eq? style 'bow))
                         y-end)
                        (else
                         (assoc-get 'Y left-bound-info))))
                 (x-length (- x-end x-start))
                 (y-height (- y-end y-start))
                 ;; We calculate the length of the stubs in X-axis direction and
                 ;; use this value to draw the stub-lines below.
                 ;; This ensures a constant printed magnitude for all gradients
                 (left-info-stub-length
                  (assoc-get 'left-stub-length left-bound-info 1))
                 (left-stub-x-y
                  (ly:directed (cons x-length y-height) left-info-stub-length))
                 (right-info-stub-length
                  (assoc-get 'right-stub-length right-bound-info 1))
                 (right-stub-x-y
                  (ly:directed (cons x-length y-height) right-info-stub-length))
                 (left-stub-stil
                  (if (and (grob::has-interface left-bound 'finger-interface)
                           (member style '(stub-left stub-both)))
                      (make-line-stencil
                       (* line-thick thick)
                       x-start
                       y-start
                       (+ x-start (car left-stub-x-y))
                       (+ y-start (cdr left-stub-x-y)))
                      empty-stencil))
                 (right-stub-stil
                  (if (and (grob::has-interface right-bound 'finger-interface)
                           (member style '(stub-right stub-both)))
                      (make-line-stencil
                       (* line-thick thick)
                       (- x-end (car right-stub-x-y))
                       (- y-end (cdr right-stub-x-y))
                       x-end
                       y-end)
                      empty-stencil)))

            (case style
              ((stub-both)
               (ly:stencil-add left-stub-stil right-stub-stil))
              ((stub-left) left-stub-stil)
              ((stub-right) right-stub-stil)
              ((bow)
               (let* ((details (ly:grob-property grob 'details))
                      ;; TODO find a more sufficient fall back
                      (fall-back (if (negative? y-start) DOWN UP))
                      ;; Go for direction modifiers, if used
                      (cause (ly:grob-property grob 'cause))
                      (dirs
                       (filter-map
                        (lambda (art)
                          (ly:prob-property art 'direction #f))
                        (ly:prob-property cause 'articulations)))
                      (dir
                       (if (and (pair? dirs) (ly:dir? (car dirs)))
                           (car dirs)
                           #f))
                      (bow-direction
                       (or (assoc-get 'bow-direction details)
                           dir
                           fall-back)))
                 (make-tie-stencil
                  (cons x-start y-start)
                  (cons x-end y-end)
                  (* line-thick thick)
                  bow-direction)))
              (else (ly:line-spanner::print grob))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; control points and polygons

(define-public ((control-point::calc-offset axis) grob)
  (let* ((bezier (ly:grob-object grob 'bezier))
         (control-points (ly:grob-property bezier 'control-points))
         (index (ly:grob-property grob 'index))
         (offset (list-ref control-points index)))
    (coord-axis offset axis)))

(define-public (control-polygon::calc-text grob)
  (let* ((bezier (ly:grob-object grob 'bezier))
         (control-points (ly:grob-property bezier 'control-points)))
    (make-polygon-markup control-points)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sticky grobs

(define-public ((sticky-grob-interface::inherit-property property) grob)
  (ly:grob-property
   (ly:grob-object grob 'sticky-host)
   property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pitched trills

;; Why not ly:axis-group-interface::pure-height?  The latter callback is
;; designed for vertical axis group spanners, filtering out grobs not in
;; the [start, end] interval.  That's not correct for TrillPitchGroup,
;; a horizontal axis group item (remember that item pure heights are assumed
;; not to depend on start and end, and cached once and for all).

;; TODO: perhaps this could be incorporated into the axis-group-interface too?
(define-public (trill-pitch-group::pure-height grob start end)
  (let* ((vertical-axis-group (ly:grob-parent grob Y))
         (elements (ly:grob-array->list (ly:grob-object grob 'elements)))
         (pure-group-extent
          (reduce interval-union
                  empty-interval
                  (map
                   (lambda (elt)
                     (ly:grob-pure-height elt vertical-axis-group 0 0))
                   elements)))
         ;; It would be crazy to make this nonzero, but anyway.
         (my-pure-coord (ly:grob-pure-property grob 'Y-offset 0 0 0.0)))
    (coord-translate pure-group-extent (- my-pure-coord))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chord grids

(define-public (chord-square::width grob)
  ;; The BarLine's X-extent is not necessarily the right measure of where to
  ;; start and stop.  For a chord square at the left of a repeat bar line ":|.",
  ;; you want to extent the square to the '|' glyph, not the ':' one.  This uses
  ;; a simple approach to determine the correct point: take a skyline of the bar
  ;; line, pad it a bit (mainly to protect against rounding errors), and take
  ;; the height at the Y positions of the two horizontal lines of the square.
  (match-let* (((low . high) (ly:grob-extent grob grob Y))
               (common (ly:grob-system grob))
               (my-coord (ly:grob-relative-coordinate grob common X)))
    (define (x-position-for-bound dir)
      (let* ((bound (ly:spanner-bound grob dir))
             (bound-coord (ly:grob-relative-coordinate bound common X))
             (basic-position (- bound-coord my-coord)))
        (if (grob::has-interface bound 'bar-line-interface)
            (let* ((skyline-pair (ly:grob-property bound 'horizontal-skylines))
                   (skyline (index-cell skyline-pair (- dir)))
                   (padded (ly:skyline-pad skyline 0.05))
                   (correction
                    ((if (eqv? dir LEFT)
                         max
                         min)
                     (ly:skyline-height padded low)
                     (ly:skyline-height padded high))))
              (+ basic-position correction))
            ;; If it's not a bar line, the bound is probably the
            ;; NonMusicalPaperColumn at the beginning of the piece; there is no
            ;; bar line there but system start delimiters (unless the original bar
            ;; line is a repeat bar line, since those *are* printed at the start
            ;; of the piece).  It's unclear what breakable items one could have
            ;; there (there are never any clefs or such in chord grids), so we
            ;; just assume that column has zero width.
            basic-position)))
    (pair-map x-position-for-bound (cons LEFT RIGHT))))

(define-public (chord-square::height grob)
  ;; If the chord square is in a \stopStaff passage, kill it.
  (let* (;; Use ly:grob-object on `grob` here since the left bound might be a
         ;; paper column, which was not created in ChordGrid (but in
         ;; ChordGridScore) and consequently doesn't have 'staff-symbol set.
         (left-staff-symbol (ly:grob-object grob 'staff-symbol #f))
         ;; On the other hand, the right bound is always a BarLine.
         (right-staff-symbol
          (let ((right (ly:spanner-bound grob RIGHT)))
            (when (not (grob::has-interface right 'bar-line-interface))
              (ly:programming-error
               "right bound of ChordSquare should be bar line, found ~a" right))
            (ly:grob-object right 'staff-symbol #f)))
         ;; The ChordSquare needs either of the two staff symbols spanning it
         ;; entirely.
         (bound-column-when
          (lambda (spanner direction)
            (let* ((bound (ly:spanner-bound spanner direction))
                   (column (ly:item-get-column bound)))
              (ly:grob-property column 'when))))
         (staff-symbol-spans-square?
          (lambda (staff-symbol direction)
            (let ((staff-symbol-when (bound-column-when staff-symbol direction))
                  (square-when (bound-column-when grob direction)))
              ((if (eqv? direction LEFT)
                   moment<=?
                   (lambda (a b)
                     (moment<=? b a)))
               staff-symbol-when
               square-when))))
         (staff-symbol-to-use
          (cond
           ((and left-staff-symbol
                 (staff-symbol-spans-square? left-staff-symbol RIGHT))
            left-staff-symbol)
           ((and right-staff-symbol
                 (staff-symbol-spans-square? right-staff-symbol LEFT))
            right-staff-symbol)
           (else
            #f))))
    (if staff-symbol-to-use
        (interval-widen (ly:grob-extent staff-symbol-to-use staff-symbol-to-use Y)
                        (- (ly:staff-symbol-line-thickness staff-symbol-to-use)))
        (begin
          (let ((chord-names (ly:grob-object grob 'chord-names #f)))
            (when chord-names
              (ly:warning
               "removing chord names on chord square because no staff symbol was found")
              (for-each ly:grob-suicide!
                        (ly:grob-array->list chord-names))))
          (ly:grob-suicide! grob)))))

(define (check-division-alist grob prop-name)
  (let ((alist (ly:grob-property grob prop-name)))
    ;; Assume `alist` is an alist, which is checked by the alist? predicate on
    ;; the property.
    (for-each
     (lambda (entry)
       (let ((key (car entry)))
         (cond
          ((not (number-list? key))
           (ly:warning "key in ~a should be numer list: ~a"
                       prop-name
                       key))
          ;; Also checks for exactness since (eqv? 1 1.0) => #f
          ;; (unlike (= 1 1.0) => #t).
          ((not (eqv? 1 (apply + key)))
           (ly:warning "key in ~a should contain exact numbers adding up to 1: ~a"
                       prop-name
                       key)))))
     alist)
    alist))

(define-public (chord-square::print grob)
  (let* ((division (ly:grob-property grob 'measure-division))
         (lines-alist (check-division-alist grob 'measure-division-lines-alist))
         (lines (or (assoc-get division lines-alist)
                    (begin
                      (ly:warning
                       (G_ "no entry in measure-division-lines-alist for measure division ~a")
                       division)
                      '())))
         (X-ext (ly:grob-extent grob grob X))
         (Y-ext (ly:grob-extent grob grob Y)))
    (apply ly:stencil-add
           (map
            (match-lambda
              ((x1 y1 x2 y2)
               (let ((scaled-x1 (interval-index X-ext x1))
                     (scaled-y1 (interval-index Y-ext y1))
                     (scaled-x2 (interval-index X-ext x2))
                     (scaled-y2 (interval-index Y-ext y2)))
                 (ly:line-interface::line grob scaled-x1 scaled-y1 scaled-x2 scaled-y2))))
            lines))))

(define-public ((grid-chord-name::calc-offset-on-axis axis) grob)
  (let* ((square (ly:grob-parent grob X))
         (division (ly:grob-property square 'measure-division))
         (placement-alist
          (check-division-alist square 'measure-division-chord-placement-alist))
         (placement (or (assoc-get division placement-alist)
                        (begin
                          (ly:warning
                           (G_ "no entry in measure-division-chord-placement-alist \
for measure division ~a")
                           division)
                          (circular-list '(0 . 0)))))
         (index (ly:grob-property grob 'index))
         (coords (list-ref placement index))
         (coord (coord-axis coords axis))
         (extent (ly:grob-extent square square axis))
         (scaled-coord (interval-index extent coord))
         (stencil (ly:grob-property grob 'stencil))
         (stencil-extent (ly:stencil-extent stencil axis))
         ;; TODO is configurability (self-alignment-{X,Y}?) called for?
         (center (interval-center stencil-extent)))
    (- scaled-coord center)))

(define-public grid-chord-name::calc-X-offset (grid-chord-name::calc-offset-on-axis X))
(define-public grid-chord-name::calc-Y-offset (grid-chord-name::calc-offset-on-axis Y))

(define-public default-measure-division-lines-alist
  '(((1) . ())
    ((1/2 1/2) . ((-1 -1 1 1)))
    ((1/2 1/4 1/4) . ((-1 -1 1 1) (0 0 1 -1)))
    ((1/4 1/4 1/2) . ((-1 -1 1 1) (-1 1 0 0)))
    ((1/4 1/4 1/4 1/4) . ((-1 -1 1 1) (-1 1 1 -1)))
    ((1/4 3/4) . ((-1 -1 0 0) (-1 1 0 0)))
    ((3/4 1/4) . ((0 0 1 -1) (0 0 1 1)))))

(define-public default-measure-division-chord-placement-alist
  '(((1) . ((0 . 0)))
    ((1/2 1/2) . ((-0.4 . 0.4) (0.4 . -0.4)))
    ((1/2 1/4 1/4) . ((-0.4 . 0.4) (0 . -0.65) (0.63 . 0)))
    ((1/4 1/4 1/2) . ((-0.63 . 0) (0 . 0.65) (0.4 . -0.4)))
    ((1/4 1/4 1/4 1/4) . ((-0.63 . 0) (0 . 0.7) (0 . -0.65) (0.63 . 0)))
    ((1/4 3/4) . ((-0.63 . 0) (0.38 . 0)))
    ((3/4 1/4) . ((-0.38 . 0) (0.63 . 0)))))

(define-public median-measure-division-lines-alist
  (assoc-set!
   (alist-copy default-measure-division-lines-alist)
   '(1/4 1/4 1/4 1/4)
   '((0 -1 0 1) (-1 0 1 0))))

(define-public median-measure-division-chord-placement-alist
  (assoc-set!
   (alist-copy default-measure-division-chord-placement-alist)
   '(1/4 1/4 1/4 1/4)
   '((-0.5 . 0.5) (0.5 . 0.5) (-0.5 . -0.5) (0.5 . -0.5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; staff highlights

(define (staff-highlight::width grob)
  (let ((refp (ly:grob-system grob)))
    (define (bound-start/end direction)
      (let ((bound (ly:spanner-bound grob direction)))
        (cond
         (((if (eqv? direction LEFT)
               not-first-broken-spanner?
               not-last-broken-spanner?)
           grob)
          (let ((ext (ly:grob-robust-relative-extent bound refp X)))
            (interval-index ext direction)))
         ((let ((break-alignment (ly:grob-object bound 'break-alignment #f)))
            (and break-alignment
                 (ly:break-alignment-interface::find-nonempty-break-align-group
                  break-alignment
                  'staff-bar)))
          => (lambda (bar-group)
               (interval-end (ly:grob-extent bar-group refp X))))
         (else
          ;; By default, don't include any prefatory material.  The simplest way
          ;; would be to take the end/start of the extent of our left bound
          ;; (NonMusicalPaperColumn), but this makes the highlight boundary look
          ;; too close to the prefatory matter, so we pad a bit with
          ;; bound-prefatory-paddings.  However, we absolutely want to avoid
          ;; having the highlight boundary cross a note head, so we cap at the
          ;; boundary with musical material.  This is given to us by the
          ;; 'columns array, which contains all PaperColumns the highlight
          ;; spans.  (Columns spanned by the original highlight but not by this
          ;; broken piece have been removed by break substitution, and the array
          ;; is ordered.)  Note that we use a paper column, not a note column.
          ;; Highlights in a staff may be influenced by the presence of
          ;; suspended notes in another staff.  This is intentional.  If there
          ;; are highlights in parallel in different staves, we want them to
          ;; start and end at the same places.
          (let ((non-musical-boundary-ext (ly:grob-extent bound refp X)))
            (if (interval-empty? non-musical-boundary-ext)
                ;; If there is no prefatory material at all, no need to shy away
                ;; from this column.
                (ly:grob-relative-coordinate bound refp X)
                (let* ((non-musical-boundary
                        (interval-index non-musical-boundary-ext (- direction)))
                       (padding (index-cell (ly:grob-property grob 'bound-prefatory-paddings)
                                            direction))
                       (columns (ly:grob-array->list (ly:grob-object grob 'columns)))
                       (musical-boundary
                        ;; Find the first/last column that actually contains
                        ;; material.  If there are skips, they don't factor into
                        ;; the highlight start and end.
                        (any
                         (lambda (col)
                           (let* ((basic-ext (ly:grob-extent col refp X))
                                  ;; The extent of a PaperColumn does not
                                  ;; include accidentals.
                                  (conditional
                                   (ly:grob-object col 'conditional-elements))
                                  (conditional-ext
                                   (ly:relative-group-extent conditional refp X))
                                  (ext
                                   (interval-union basic-ext conditional-ext)))
                             (and (not (interval-empty? ext))
                                  (interval-index ext direction))))
                         (if (eqv? direction LEFT)
                             ;; search from left
                             columns
                             ;; search from right
                             (reverse columns))))
                       (wished-coord ((if (eqv? direction LEFT)
                                          +
                                          -)
                                      non-musical-boundary
                                      padding)))
                  (if (or (not musical-boundary)
                          ((if (eqv? direction LEFT)
                               <=
                               >=)
                           wished-coord
                           musical-boundary))
                      wished-coord
                      musical-boundary))))))))
    (let ((base-position (ly:grob-relative-coordinate grob refp X))
          (start (bound-start/end LEFT))
          (end (bound-start/end RIGHT))
          (shorten-pair (ly:grob-property grob 'shorten-pair)))
      (cons
       (- (+ start (car shorten-pair))
          base-position)
       (- (- end (cdr shorten-pair))
          base-position)))))

(define (staff-highlight::height grob)
  (let* ((staff-symbol-array (ly:grob-object grob 'elements))
         (refp (ly:grob-common-refpoint-of-array grob staff-symbol-array Y)))
    ;; Combine StaffSymbol heights.  Use the same "widened"
    ;; extents as bar lines.
    (fold (lambda (staff-symbol height-so-far)
            (interval-union
             height-so-far
             (let ((ext (ly:grob-property staff-symbol 'widened-extent))
                   (coord (ly:grob-relative-coordinate staff-symbol refp Y)))
               (coord-translate ext coord))))
          empty-interval
          (ly:grob-array->list staff-symbol-array))))

(define (staff-highlight::print grob)
  (let* ((width (ly:grob-extent grob grob X))
         (height (ly:grob-extent grob grob Y)))
    (make-filled-box-stencil width height)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text marks

(define-public (text-mark-interface::calc-break-visibility grob)
  (let* ((ev (event-cause grob))
         (is-end-mark (eqv? LEFT (ly:event-property ev 'horizontal-direction))))
    (if is-end-mark begin-of-line-invisible end-of-line-invisible)))

(define-public (text-mark-interface::calc-self-alignment-X grob)
  ;; \textEndMark aligns on the right, as it's intended for text that look back
  ;; at the preceding music, and is visible at the end of a line.  \textMark
  ;; aligns on the left; text marks are often long (in the sense of "longer than
  ;; rehearsal marks, which are just one letter"), so centering doesn't look
  ;; very well.  That said, text marks fit various exotic use cases, so it is to
  ;; be expected that the user will have to tweak self-alignment-X sometimes.
  (let* ((ev (event-cause grob))
         (is-end-mark (eqv? LEFT (ly:event-property ev 'horizontal-direction))))
    (if is-end-mark
        RIGHT
        LEFT)))
