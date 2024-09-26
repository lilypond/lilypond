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


;; for more control over glyph-name calculations,
;; we use a custom callback for tab note heads
;; which will ignore 'style = 'do
(define-public (tab-note-head::calc-glyph-name grob)
  (let ((style (ly:grob-property grob 'style)))

    (case style
      ((cross) "2cross")
      ((slash) "2slash")
      (else #f))))

;; ensure we only call note head callback when
;; style is set to a known value
(define-public (tab-note-head::whiteout-if-style-set grob)
  (let ((style (ly:grob-property grob 'style)))

    (case style
      ((cross slash) (stencil-whiteout-box (ly:note-head::print grob)))
      (else (tab-note-head::print grob)))))

;; definitions for the "moderntab" clef:
;; the "moderntab" clef will be added to the list of known clefs,
;; so it can be used as any other clef: \clef "moderntab"
(add-new-clef "moderntab" "markup.moderntab" 0 0 0)

;; define sans serif-style tab-Clefs as a markup:
(define-markup-command (customTabClef
                        layout props num-strings staff-space)
  (integer? number?)
  #:category music
  "Draw a clef in sans-serif style for a tablature with @var{num-strings} lines
spaced by @var{staff-space}.

This markup command is used to implement @code{\\clef moderntab} within a
@code{TabStaff} context.

@lilypond[verbatim,quote]
\\markup {
  \\customTabClef #4 #1
}
@end lilypond"
  (define (square x) (* x x))
  (let* ((scale-factor (/ staff-space 1.5))
         (font-size (- (* num-strings 1.5 scale-factor) 7))
         (base-skip (* (square (+ (* num-strings 0.195) 0.4)) scale-factor)))

    (interpret-markup layout props
                      (make-vcenter-markup
                       (make-bold-markup
                        (make-override-markup
                         '(font-family . sans)
                         (make-fontsize-markup
                          font-size
                          (make-override-markup
                           `(baseline-skip . ,base-skip)
                           (make-left-align-markup
                            (make-center-column-markup
                             '("T" "A" "B")))))))))))

;; this function decides which clef to take
(define-public (clef::print-modern-tab-if-set grob)
  (let ((glyph (ly:grob-property grob 'glyph)))

    ;; which clef is wanted?
    (if (string=? glyph "markup.moderntab")
        ;; if it is "moderntab", we'll draw it
        (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
               (line-count (if (ly:grob? staff-symbol)
                               (ly:grob-property staff-symbol 'line-count)
                               0))
               (staff-space (ly:staff-symbol-staff-space grob)))

          (grob-interpret-markup grob (make-customTabClef-markup line-count
                                                                 staff-space)))
        ;; otherwise, we simply use the default printing routine
        (ly:clef::print grob))))

;; if stems are drawn, it is nice to have a double stem for
;; (dotted) half notes to distinguish them from quarter notes:
(define-public (tabvoice::make-double-stem-width-for-half-notes grob)
  (let ((X-extent (ly:stem::width grob)))
    ;; does the stem exist and is it on a (dotted) half note?
    (if (and (not (equal? X-extent empty-interval))
             (= 1 (ly:grob-property grob 'duration-log)))

        ;; yes -> return double stem X-extent
        (let* ((single-stem-width (- (cdr X-extent) (car X-extent)))
               (separation (ly:grob-property grob 'double-stem-separation 0.5))
               (total-width (+ single-stem-width separation))
               (half-width (/ total-width 2)))
          (cons (- half-width) half-width))
        ;; no -> return simple stem X-extent
        X-extent)))

(define-public (tabvoice::draw-double-stem-for-half-notes grob)
  (let ((stem-stencil (ly:stem::print grob)))
    ;; does the stem exist and is it on a (dotted) half note?
    (if (and (ly:stencil? stem-stencil)
             (= 1 (ly:grob-property grob 'duration-log)))

        ;; yes -> draw double stem
        (let* ((separation (ly:grob-property grob 'double-stem-separation 0.5))
               (half-separation (/ separation 2)))
          (ly:stencil-add
           (ly:stencil-translate-axis stem-stencil (- half-separation) X)
           (ly:stencil-translate-axis stem-stencil half-separation X)))
        ;; no -> draw simple stem (or none at all)
        stem-stencil)))

;; as default, the glissando line between fret numbers goes
;; upwards, here we have a function to correct this behavior:
(define-public (glissando::calc-tab-extra-dy grob)
  (let* ((original (ly:grob-original grob))
         (left-bound (ly:spanner-bound original LEFT))
         (right-bound (ly:spanner-bound original RIGHT))
         (left-pitch (ly:event-property (event-cause left-bound) 'pitch))
         (right-pitch (ly:event-property (event-cause right-bound) 'pitch)))

    (if (< (ly:pitch-tones right-pitch) (ly:pitch-tones left-pitch))
        -0.75
        0.75)))

;; The slurs should not be too far apart from the corresponding fret number, so
;; we move the slur towards the TabNoteHeads.
(define-public slur::move-closer-to-tab-note-heads
  ;; TODO: use a less "brute-force" method to decrease
  ;; the distance between the slur ends and the fret numbers
  (grob-transformer
   'control-points
   (lambda (grob control-points)
     (let ((direction (ly:grob-property grob 'direction))
           (staff-space (ly:staff-symbol-staff-space grob)))
       (map
        (lambda (p)
          (cons (car p)
                (- (cdr p)
                   (* staff-space direction 0.35))))
        control-points)))))

;; The glissando routine works similarly to the slur routine; if the
;; fret number is "tied to", it should become parenthesized.
(define-public (glissando::draw-tab-glissando grob)
  (let* ((original (ly:grob-original grob))
         (left-tab-note-head (ly:spanner-bound original LEFT))
         (cautionary (ly:grob-property left-tab-note-head 'parenthesized #f)))

    (and cautionary
         ;; increase left padding to avoid collision between
         ;; closing parenthesis and glissando line
         (ly:grob-set-nested-property! grob '(bound-details left padding) 0.5))
    (ly:line-spanner::print grob)))

;; for \tabFullNotation, the stem tremolo beams are too big in comparison to
;; normal staves; this wrapper function scales accordingly:
(define-public (stem-tremolo::calc-tab-width grob)
  (let ((width (ly:stem-tremolo::calc-width grob))
        (staff-space (ly:staff-symbol-staff-space grob)))
    (/ width staff-space)))


;; a callback for custom fret labels
(define-public ((tab-note-head::print-custom-fret-label fret) grob)
  (ly:grob-set-property! grob 'text (make-vcenter-markup fret))
  (tab-note-head::print grob))


(define-public (tab-note-head::handle-ties grob)
  "Handle tab note heads ending a @code{Tie}, deal with @code{RepeatTie}.

If a @code{Slur} or @code{Glissando} starts at a tie-ending @code{TabNoteHead}
always print the @code{TabNoteHead} parenthesized.

If a tie-ending @code{TabNoteHead} occurs at the beginning of a line print it
parenthesized unless sub-properties @code{note-head-visible} and
@code{parenthesize} of @code{details.tied-properties} are set @code{#f}, which
can be done manually or using @code{hideSplitTiedTabNotes}.

A @code{TabNoteHead} with @code{\\repeatTie} is printed parenthesized as well,
the same holds if @code{\\repeatTie} is applied to a chord. This is useful for
seconda volta blocks. This behaviour can be switched off with
@code{hideSplitTiedTabNotes}."
;; TODO move behaviour for repeatTie and 'span-start to the stencil-procedure?
  (let* (;; get PaperColumn
         (pc (ly:item-get-column grob))
         ;; get NonMusicalPaperColumn
         (nmpc (ly:grob-object pc 'left-neighbor #f))
         (at-line-begin? (and nmpc (positive? (ly:item-break-dir nmpc))))
         (span-start? (ly:grob-property grob 'span-start #f))
         (details (ly:grob-property grob 'details '()))
         (tied-props (assoc-get 'tied-properties details '()))
         (tied? (assoc-get 'tied tied-props #f))
         (repeat-tied? (assoc-get 'repeat-tied tied-props #f))
         (tied-visible? (assoc-get 'note-head-visible tied-props #t))
         (tied-parenthesized? (assoc-get 'parenthesize tied-props #t)))

    (cond ((or span-start?
               (and repeat-tied? tied-visible? tied-parenthesized?)
               (and tied? at-line-begin? tied-visible? tied-parenthesized?))
             (ly:grob-set-property! grob 'parenthesized #t)
             ;; The stencil procedure needs to be run again, otherwise
             ;; 'dparenthesized would have no effect.
             (ly:grob-set-property! grob 'stencil tab-note-head::print))
          ((or tied? repeat-tied?)
            (ly:grob-set-property! grob 'transparent #t)))))

(define-public (tab-note-head::print grob)
  "Print a tab note head."
  (define (is-harmonic? grob)
    (let ((arts (ly:event-property (event-cause grob) 'articulations)))
      (or (pair? (filter (lambda (a)
                           (ly:in-event-class? a 'harmonic-event))
                         arts))
          (eq? (ly:grob-property grob 'style) 'harmonic))))

  (let* ((cautionary (ly:grob-property grob 'parenthesized #f))
         (details (ly:grob-property grob 'details '()))
         (harmonic-props (assoc-get 'harmonic-properties details '()))
         (harmonic-angularity (assoc-get 'angularity harmonic-props 2))
         (harmonic-half-thick (assoc-get 'half-thickness harmonic-props 0.075))
         (harmonic-padding (assoc-get 'padding harmonic-props 0))
         (harmonic-proc (assoc-get 'procedure harmonic-props parenthesize-stencil))
         (harmonic-width (assoc-get 'width harmonic-props 0.25))
         (cautionary-props (assoc-get 'cautionary-properties details '()))
         (cautionary-angularity (assoc-get 'angularity cautionary-props 2))
         (cautionary-half-thick (assoc-get 'half-thickness cautionary-props 0.075))
         (cautionary-padding (assoc-get 'padding cautionary-props 0))
         (cautionary-proc (assoc-get 'procedure cautionary-props parenthesize-stencil))
         (cautionary-width (assoc-get 'width cautionary-props 0.25))
         (output-grob (ly:text-interface::print grob))
         (ref-grob (grob-interpret-markup grob "8"))
         (offset-factor (assoc-get 'head-offset details 3/5))
         (column-offset (* offset-factor
                           (interval-length
                            (ly:stencil-extent ref-grob X)))))

    (if (is-harmonic? grob)
        (set! output-grob (harmonic-proc output-grob
                                         harmonic-half-thick
                                         harmonic-width
                                         harmonic-angularity
                                         harmonic-padding)))
    (if cautionary
        (set! output-grob (cautionary-proc output-grob
                                           cautionary-half-thick
                                           cautionary-width
                                           cautionary-angularity
                                           cautionary-padding)))

    (ly:stencil-translate-axis
     (ly:stencil-aligned-to output-grob X CENTER)
     column-offset
     X)))

;; Harmonic definitions

(define node-positions
  ;; for the node on m/n-th of the string length, we get the corresponding
  ;; (exact) fret position by calculating p=(-12/log 2)*log(1-(m/n));
  ;; since guitarists normally use the forth fret and not the 3.8th, here
  ;; are rounded values, ordered by
  ;; 1/2
  ;; 1/3 2/3
  ;; 1/4 2/4 3/4 etc.
  ;; The value for 2/4 is irrelevant in practical, bacause the string sounds
  ;; only one octave higher, not two, but since scheme normalizes the fractions
  ;; anyway, these values are simply placeholders for easier indexing.
  ;; According to the arithmetic sum, the position of m/n is at 1/2*(n-2)(n-1)+(m-1)
  ;; if we start counting from zero
  (vector 12
          7   19
          5   12    24
          4    9    16   28
          3    7    12   19    31
          2.7  5.8  9.7  14.7  21.7  33.7
          2.3  5    8    12    17    24    36
          2    4.4  7    10    14    19    26  38 ))

(define partial-pitch
  (vector '(0 0 0)
          '(1 0 0)
          '(1 4 0)
          '(2 0 0)
          '(2 2 0)
          '(2 4 0)
          '(2 6 -1/2)
          '(3 0 0)
          '(3 1 0)))

(define fret-partials
  '(("0" . 0)
    ("12" . 1)
    ("7" . 2)
    ("19" . 2)
    ("5" . 3)
    ("24" . 3)
    ("4" . 4)
    ("9" . 4)
    ("16" . 4)
    ("3" . 5)
    ("2.7" . 6)
    ("2.3" . 7)
    ("2" . 8)))

(define-public (ratio->fret ratio)
  "Calculate a fret number given @var{ratio} for the harmonic."
  (let* ((nom (numerator ratio))
         (den (denominator ratio))
         (index (+ (* (- den 2)
                      (- den 1)
                      1/2)
                   nom -1)))
    (number->string (vector-ref node-positions index))))

(define-public (ratio->pitch ratio)
  "Calculate a pitch given @var{ratio} for the harmonic."
  (let* ((partial (1- (denominator ratio)))
         (pitch (vector-ref partial-pitch partial)))

    (ly:make-pitch (first pitch)
                   (second pitch)
                   (third pitch))))

(define-public (fret->pitch fret)
  "Calculate a pitch given @var{fret} for the harmonic."
  (let* ((partial (assoc-get fret fret-partials 0))
         (pitch (vector-ref partial-pitch partial)))

    (ly:make-pitch (first pitch)
                   (second pitch)
                   (third pitch))))

(define-public (calc-harmonic-pitch pitch music)
  "Calculate the harmonic pitches in @var{music} given
@var{pitch} as the non-harmonic pitch."
  (let ((es (ly:music-property music 'elements))
        (e (ly:music-property music 'element))
        (p (ly:music-property music 'pitch)))
    (cond
     ((pair? es)
      (ly:music-set-property! music 'elements
                              (map (lambda (x) (calc-harmonic-pitch pitch x)) es)))
     ((ly:music? e)
      (ly:music-set-property! music 'element (calc-harmonic-pitch pitch e)))
     ((ly:pitch? p)
      (begin
        (set! p (+ p pitch))
        (ly:music-set-property! music 'pitch p))))
    music))

(define-public (make-harmonic mus)
  "Convert music variable @var{mus} to harmonics."
  (let ((elts (ly:music-property mus 'elements))
        (elt (ly:music-property mus 'element)))
    (cond
     ((pair? elts)
      (for-each make-harmonic elts))
     ((ly:music? elt)
      (make-harmonic elt))
     ((music-is-of-type? mus 'note-event)
      (set! (ly:music-property mus 'articulations)
            (append
             (ly:music-property mus 'articulations)
             (list (make-music 'HarmonicEvent))))))
    mus))
