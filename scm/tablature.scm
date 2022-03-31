;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2009--2022 Marc Hohl <marc@hohlart.de>
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
  "Draw a tab clef sans-serif style."
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

;; FIXME: some of the below routines have side effects that sound
;; like they could cause dependency on callback order. --JeanAS

;; the handler for ties in tablature; according to TabNoteHead #'details,
;; the 'tied to' note is handled differently after a line break
(define-public (tie::handle-tab-note-head grob)
  (let* ((original (ly:grob-original grob))
         (tied-tab-note-head (ly:spanner-bound grob RIGHT))
         (spanner-start (ly:grob-property tied-tab-note-head 'span-start #f))
         (siblings (if (ly:grob? original)
                       (ly:spanner-broken-into original) '())))

    (if spanner-start
        ;; tab note head is right bound of a tie and left of spanner,
        ;; -> parenthesize it at all events
        (begin
          (ly:grob-set-property! tied-tab-note-head 'display-cautionary #t)
          (ly:grob-set-property! tied-tab-note-head 'stencil tab-note-head::print))
        ;; otherwise, check whether tie is split:
        (if (and (>= (length siblings) 2)
                 (eq? (car (last-pair siblings)) grob))
            ;; tie is split -> get TabNoteHead #'details
            (let* ((details (ly:grob-property tied-tab-note-head 'details))
                   (tied-properties (assoc-get 'tied-properties details '()))
                   (tab-note-head-parenthesized (assoc-get 'parenthesize tied-properties #t)))

              (if tab-note-head-parenthesized
                  (begin
                    (ly:grob-set-property! tied-tab-note-head 'display-cautionary #t)
                    (ly:grob-set-property! tied-tab-note-head 'stencil tab-note-head::print))))

            ;; tie is not split
            (ly:grob-set-property! tied-tab-note-head 'transparent #t)))))



;; repeat ties occur within alternatives in a repeat construct;
;; TabNoteHead #'details handles the appearance in this case
(define-public (repeat-tie::handle-tab-note-head grob)
  (let* ((tied-tab-note-head (ly:grob-object grob 'note-head))
         (spanner-start (ly:grob-property tied-tab-note-head 'span-start #f)))
    (if spanner-start
        ;; tab note head is between a tie and a slur/glissando
        ;; -> parenthesize it at all events
        (begin
          (ly:grob-set-property! tied-tab-note-head 'display-cautionary #t)
          (ly:grob-set-property! tied-tab-note-head 'stencil tab-note-head::print))
        ;; otherwise check 'details
        (let* ((details (ly:grob-property tied-tab-note-head 'details))
               (repeat-tied-properties (assoc-get 'repeat-tied-properties details '()))
               (tab-note-head-visible (assoc-get 'note-head-visible repeat-tied-properties #t))
               (tab-note-head-parenthesized (assoc-get 'parenthesize repeat-tied-properties #t)))

          (if tab-note-head-visible
              ;; tab note head is visible
              (if tab-note-head-parenthesized
                  (begin
                    (ly:grob-set-property! tied-tab-note-head 'display-cautionary #t)
                    (ly:grob-set-property! tied-tab-note-head 'stencil tab-note-head::print)))
              ;; tab note head is invisible
              (ly:grob-set-property! tied-tab-note-head 'transparent #t))))))

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
         (cautionary (ly:grob-property left-tab-note-head 'display-cautionary #f)))

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

(define-public (tab-note-head::print grob)
  (define (is-harmonic? grob)
    (let ((arts (ly:event-property (event-cause grob) 'articulations)))
      (or (pair? (filter (lambda (a)
                           (ly:in-event-class? a 'harmonic-event))
                         arts))
          (eq? (ly:grob-property grob 'style) 'harmonic))))

  (let* ((cautionary (ly:grob-property grob 'display-cautionary #f))
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
        (set! p (ly:pitch-transpose p pitch))
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
