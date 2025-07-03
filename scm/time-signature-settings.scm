;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2009--2023 Carl Sorensen <c_sorensen@byu.edu>
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

;;; specify time signature default settings

;;; format:
;;;
;;; alist of
;;;   (time-signature . default-properties) entries.
;;;
;;; where default-properties is an alist containing information about the
;;; time signature.  Each default-properties set can contain the
;;; following entries:
;;;
;;;   (beatBase . (/ numerator denominator))
;;;   (beatStructure . structure-list)
;;;   (beamExceptions . (alist of beam exceptions that don't follow beats))
;;;
;;; The alist of beam exceptions has the following entries:
;;;
;;;   (end . grouping-rules)
;;;   (subdivide . grouping-rules)  (not yet implemented, reserved for future use)
;;;
;;;   grouping-rules is an alist containing (beam-type . grouping-list) entries
;;;
;;;     beam-type is the length as a rational number
;;;     grouping-list is a list that specifies the
;;;     number of stems of the given duration that are grouped in a beamed unit.
;;;     For an exception, the duration used is beam-type.  For measureBeats,
;;;     the duration used is beatBase.
;;;
;;;     If an exception is specified for a given beam-type, it will apply to all
;;;     beams of shorter durations that don't have an individual exception, so
;;;     (1/8 . (3 3 2))
;;;     will cause all primary beams to be broken at 3/8, 6/8, and 8/8.
;;;
;;;     (1/32 . (16 8 4 4))
;;;     will cause all 1/32, 1/64, and 1/128 beams to be broken at 1/2, 3/4,
;;;     7/8, and 8/8.
;;;
;;;     Tuplets are referenced using their actual (scaled) length, so
;;;     a 3/2 tuplet of the 1/8 kind would get exceptions looked up
;;;     under 1/12.
;;;
;;; If no values are given for beatBase and measureBeats, default values
;;;   will be assigned:
;;;   beatBase gets the value (/ time-signature-denominator)
;;;   beatStructure gets a list of (3 3 3 ...), where the number of entries is the
;;;     number of beats, each containing 3 base-moments, if the time
;;;     signature numerator is greater than 3 and divisible by 3, and
;;;     a list of (1 1 1 ...), where the number of entries is the
;;;     number of base moments in a measure otherwise.

(define-public default-time-signature-settings
  '(
    ;; in 2/2 time:
    ;;   use defaults, but end beams with 32nd notes each 1 4 beat
    ((2 . 2) .
     ((beamExceptions . ((end . ((1/32 . (8 8 8 8))))))))

    ;; in 2/4, 2/8 and 2/16 time:
    ;;   use defaults, so no entries are necessary

    ;; in 3 2 time:
    ;;   use defaults, but end beams with 32nd notes and higher each 1 4 beat

    ((3 . 2) .
     ((beamExceptions . ((end .  ((1/32 . (8 8 8 8 8 8))))))))

    ;; in 3 4 time:
    ;;   use defaults, but combine all beats into a unit if possible
    ;;
    ;;   set all beams to end on beats, but 1 8 to beam entire measure
    ;;   in order to avoid beaming every beam type for the entire measure, we set
    ;;   triplets back to every beat.
    ((3 . 4) .
     ((beamExceptions . ((end . ((1/8 . (6))            ;1/8 note whole measure
                                 (1/12 . (3 3 3)))))))) ;Anything shorter by beat

    ;; in 3 8  time:
    ;;   beam entire measure together
    ((3 . 8) . ((beamExceptions . ((end . ((1/8 . (3))))))))

    ;; in 3 16 time:
    ;;   use defaults -- no entries necessary

    ;; in 4 2 time:
    ;;   use defaults, but end beams with 16th notes or finer each 1 4 beat
    ((4 . 2) .
     ((beamExceptions . ((end . ((1/16 . (4 4 4 4 4 4 4 4))))))))

    ;; in 4 4 (common) time:
    ;;   use defaults, but combine beats 1,2 and 3,4 if only 8th notes
    ;;   NOTE: Any changes here need to be duplicated in
    ;;         ly/engraver-init.ly where the default time signature is set
    ;;         are set
    ((4 . 4) .
     ((beamExceptions . ((end . ((1/8 . (4 4))  ; 1/8 notes half measure
                                 (1/12 . (3 3 3 3)))))))) ;Anything shorter by beat

    ;; in 4/8 time:
    ;;   combine beats 1 and 2, so beam in 2
    ((4 . 8) . ((beatStructure . (2 2))))

    ;; in 4/16 time:
    ;;   use defaults, so no entries necessary

    ;; in 6 4 time:
    ;;   use defaults, but end beams with 32nd or finer each 1/4 beat
    ((6 . 4) .
     ((beamExceptions . ((end .  ((1/16 . (4 4 4 4 4 4))))))))

    ;; in 6 8 time:
    ;;   use defaults, so no entries necessary

    ;; in 6 16 time:
    ;;   use defaults, so no entries necessary

    ;; in 9 4 time:
    ;;   use defaults, but end beams with 32nd or finer each 1 4 beat
    ((9 . 4) .
     ((beamExceptions . ((end . ((1/32 . (8 8 8 8 8 8 8 8))))))))

    ;; in 9 8 time
    ;;   use defaults, so no entries necessary

    ;; in 9 16 time
    ;;   use defaults, so no entries necessary

    ;; in 12 4 time:
    ;;   use defaults, but end beams with 32nd or finer notes each 1 4 beat
    ((12 . 4) .
     ((beamExceptions . ((end . ((1/32 . (8 8 8 8 8 8 8 8 8 8 8 8))))))))

    ;; in 12 8 time:
    ;;   use defaults, so no entries necessary

    ;; in 12 16 time:
    ;;   use defaults, so no entries necessary

    ;; in 5 8 time:
    ;;   default: group (3 2)
    ((5 . 8) .
     ((beatStructure . (3 2))))

    ;; in 8 8 time:
    ;;   default: group (3 3 2)
    ((8 . 8) .
     ((beatStructure . (3 3 2))))

    ))  ; end of alist definition

;;;
;;;  Accessor and constructor functions
;;;

(define (tsig-fraction-abbr-expand abbr)
  "Syntactically convert an abbreviated time-signature fraction to
canonical form, or to @code{#f} on error.  Values are not examined."
  (define (numerator-term-expand x)
    (if (pair? x)
        (tsig-fraction-abbr-expand x)
        x))

  (define (numerator-expand num)
    (if (null? (cdr num))
        (numerator-term-expand (car num)) ; one term
        (let ((terms (map numerator-term-expand num))) ; two or more terms
          (and (every identity terms) terms))))

  (and (list? abbr)
       (not (null? (cdr abbr))) ; need at least two elements
       (let* ((reversed (reverse abbr))
              (den (car reversed)))
         (and (not (pair? den))
              (let ((num (numerator-expand (reverse (cdr reversed)))))
                (and num
                     (cons num den)))))))

(define-public (tsig-abbr-expand abbr)
  "Convert abbreviated time signature @var{abbr} to canonical form.

This change is purely syntactic and does not check any values.  Return @code{#f}
if @var{abbr} is not syntactically valid.

See the @code{\\compoundMeter} command for a description of abbreviated form.
In canonical form,

@itemize
@item
The concatenation of two or more elements is represented by a list.  There are
no single-element lists.
@item
A fraction is represented by a pair, @code{(@var{numerator}
.@tie{}@var{denominator})}, where the denominator is not a pair.
@item
A time signature is a fraction or a list of them.
@end itemize

Example:

@example
(tsig-abbr-expand '((2 3 8) (2 4)))
@result{} '(((2 3) . 8) (2 . 4))
@end example
"
  (or (tsig-fraction-abbr-expand abbr)
      (and (list? abbr)
           (if (null? (cdr abbr))
               ;; one fraction in a list: discard the outer list
               (tsig-fraction-abbr-expand (car abbr))
               ;; two or more fractions: convert each
               (let ((fracs (map tsig-fraction-abbr-expand abbr)))
                 (and (every identity fracs) ; verify conversion
                      fracs))))))

(define (get-setting my-symbol time-signature time-signature-settings)
  "Get setting @code{my-symbol} for @code{time-signature} from
@code{time-signature-settings}."
  (let ((my-time-signature-settings
         (assoc-get time-signature time-signature-settings '())))
    (assoc-get my-symbol my-time-signature-settings '())))

(define-public (make-setting beat-base
                             beat-structure
                             beam-exceptions)
  (list
   (cons 'beatBase (musical-length->number beat-base))
   (cons 'beatStructure beat-structure)
   (cons 'beamExceptions beam-exceptions)))

(define-public (calc-measure-length time-signature)
  "Calculate the measure length for @var{time-signature}."
  (if (pair? time-signature)
      (/ (car time-signature)
         (if (zero? (cdr time-signature))
             0.0 ; avoid integer div error
             (cdr time-signature)))
      +inf.0)) ;; senza misura

(define-public (beat-base time-signature time-signature-settings)
  "Get @code{beatBase} rational value for @var{time-signature} from
@var{time-signature-settings}."
  (let ((return-value (get-setting 'beatBase
                                   time-signature
                                   time-signature-settings)))
    (if (null? return-value)
        (if (pair? time-signature)
            (/ (if (zero? (cdr time-signature))
                   0.0 ; avoid integer div error
                   (cdr time-signature)))
            +inf.0) ; senza misura
        return-value)))

(define-public (beat-structure base time-sig time-signature-settings)
  "Get the @code{beatStructure} value for time signature @var{time-sig} from
@var{time-signature-settings}, scaled to @var{base} units.  If there is no
entry, derive a structure from the time signature."

  (define (default-beat-structure time-sig)
    (let* ((num (car time-sig))
           (group-size (if (and (> num 3)
                                (zero? (remainder num 3)))
                           3
                           1))
           (beat-count (/ num group-size)))
      (if (integer? beat-count)
          (make-list beat-count group-size)
          ;; There's no necessarily correct default placement of the partial
          ;; beat.  We place it at the end for the mnemonic value of matching
          ;; how a mixed number is written, e.g., "4 1/2."
          (let ((part (- beat-count (floor beat-count)))
                (full (make-list (floor beat-count) group-size)))
            (reverse (cons part full))))))
  (if (not (finite? base))
      '() ; senza misura: scaling the structure (if any) is meaningless
      (let* ((settings (assoc-get time-sig time-signature-settings '()))
             (table-base (assoc-get 'beatBase settings '()))
             (beat-factor (/ base
                             (if (null? table-base)
                                 (beat-base time-sig time-signature-settings)
                                 table-base)))
             (table-struct (assoc-get 'beatStructure settings '()))
             (struct (if (and (null? table-struct) (pair? time-sig))
                         (default-beat-structure time-sig)
                         table-struct)))
        (if (zero? beat-factor)
            ;; Likely: The computed beat base is +inf.0 for senza misura.
            ;; Unexpected: The beat base found in the settings is +inf.0
            ;; or the beat base function argument is zero.
            '(+inf.0)
            (map (lambda (x) (/ x beat-factor)) struct)))))

(define-public (beam-exceptions time-signature time-signature-settings)
  "Get @code{beamExceptions} value for @var{time-signature} from
@var{time-signature-settings}."
  (get-setting 'beamExceptions time-signature time-signature-settings))


;;; Functions for overriding time-signature settings
;;;

(define (override-property-setting context property setting value)
  "Like the C++ code that executes \\override, but without type
checking."
  (begin
    (ly:context-set-property!
     context
     property
     (cons (cons setting value) (ly:context-property context property)))))

(define (revert-property-setting context property setting)
  "Like the C++ code that executes \revert, but without type
checking."

  (define (entry-count alist entry-key)
    "Count the number of entries in alist with a key of
ENTRY-KEY."
    (cond
     ((null? alist) 0)
     ((equal? (caar alist) entry-key)
      (+ 1 (entry-count (cdr alist) entry-key)))
     (else (entry-count (cdr alist) entry-key))))

  (define (revert-member alist entry-key)
    "Return ALIST, with the first entry having a key of
ENTRY-KEY removed.  ALIST is not modified, instead
a fresh copy of the list-head is made."
    (cond
     ((null? alist) '())
     ((equal? (caar alist) entry-key) (cdr alist))
     (else (cons (car alist)
                 (revert-member (cdr alist) entry-key)))))

  ;; body of revert-property-setting
  (let ((current-value (ly:context-property context property)))
    (if (> (entry-count current-value setting) 0)
        (ly:context-set-property!
         context
         property
         (revert-member current-value setting)))))

(define-public (override-time-signature-setting time-signature setting)
  "Override the time signature settings for the context in
@var{time-signature}, with the new setting alist @var{setting}."
  (context-spec-music
   (make-apply-context
    (lambda (c) (override-property-setting
                 c
                 'timeSignatureSettings
                 time-signature
                 setting)))
   'Timing))

(define-public (revert-time-signature-setting time-signature)
  (context-spec-music
   (make-apply-context
    (lambda (c)
      (revert-property-setting
       c
       'timeSignatureSettings
       time-signature)))
   'Timing))


;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Formatting of complex/compound time signatures

;; There ought to be a \join-line sep {...} command
(define (insert-markups l m)
  (let ((ll (reverse l)))
    (let join-markups ((markups (list (car ll)))
                       (remaining (cdr ll)))
      (if (pair? remaining)
          (join-markups (cons (car remaining)
                              (cons m markups))
                        (cdr remaining))
          markups))))

(define-markup-command (compound-meter layout props time-sig)
  (number-or-pair?)
  #:category music
  #:properties ((denominator-style 'default)
                (font-size 0)
                (nested-fraction-mixed #t)
                (nested-fraction-orientation 'default)
                (nested-fraction-relative-font-size '())
                (note-dots-direction CENTER)
                (note-flag-style '())
                (note-head-style '())
                (note-staff-position -2))
  "Draw a numeric time signature based on @var{time-sig}.

@var{time-sig} can be a single number, a pair of numbers, a simple list, or a
list of lists, as the following example demonstrates.

@lilypond[verbatim,quote]
\\markuplist {
  \\override #'(baseline-skip . 4.5)
  \\override #'(padding . 4.5)
  \\table #'(-1 -1) {
    \"Single number\"  \\compound-meter #3
    \"Conventional\"   \\line {
                       \\compound-meter #'(4 . 4) or
                       \\compound-meter #'(4 4)
                     }
    \"Subdivided\"     \\compound-meter #'(2 3 5 8)
    \"Alternating\"    \\line {
                       \\compound-meter #'((2) (3)) or
                       \\compound-meter #'((2 3 8) (3 4))
                     }
  }
}
@end lilypond

Setting the @code{denominator-style} property to @code{note} prints
denominators as a note and dots when exact representation is possible.  Example:

@lilypond[verbatim,quote]
\\markup {
  \\override #'(denominator-style . note)
  \\line {
    \\compound-meter #'(2 2) or
    \\compound-meter #'(4 1/2) or
    \\compound-meter #'((2 8/3) (3 4)) but not
    \\compound-meter #'(8 20)
  }
}
@end lilypond

The @code{nested-fraction-mixed} property controls whether fractional parts are
printed as mixed numbers or as common fractions.  Example:

@lilypond[verbatim,quote]
\\markup {
  \\override #'(nested-fraction-mixed . #f)
  \\compound-meter #'(5/2 4) or
  \\override #'(nested-fraction-mixed . #t)
  \\compound-meter #'(5/2 4)
}
@end lilypond

The @code{nested-fraction-orientation} property controls how nested fractions
are arranged.  Supported values are @code{horizontal} and @code{vertical}.
Example:

@lilypond[verbatim,quote]
\\markup {
  \\override #'(nested-fraction-orientation . horizontal)
  \\compound-meter #'(5/2 4) or
  \\override #'(nested-fraction-orientation . vertical)
  \\compound-meter #'(5/2 4)
}
@end lilypond

The @code{nested-fraction-relative-font-size} property controls the size of the
numerals in nested fractions.  Recommended values are -5.5 and 0.  Using large
numerals may take precedence over related properties.  Example:

@lilypond[verbatim,quote]
\\markup {
  \\override #'(nested-fraction-relative-font-size . -5.5)
  \\compound-meter #'(5/2 4) or
  \\override #'(nested-fraction-relative-font-size . 0)
  \\compound-meter #'(5/2 4)
}
@end lilypond
"
  (define (format-mixed-fraction-horiz rational)
    (let* ((ipart (truncate rational))
           (fpart (- rational ipart)))
      (if (zero? ipart)
          (number->string fpart)
          (string-append (number->string ipart) " " (number->string fpart)))))

  (define (format-small-fraction-horiz rational relative-staff-position)
    ;; Assume that the normal time-signature digit occupies two staff spaces.
    ;; The vertical center is therefore at +1 staff space; that is the reference
    ;; point for relative-staff-position.
    (make-fontsize-markup
     (if (number? nested-fraction-relative-font-size)
         nested-fraction-relative-font-size
         -5.5)
     (make-translate-markup
      (cons
       0 ; X
       (* (1+ (/ relative-staff-position 2)) ; Y
          (ly:output-def-lookup layout 'staff-space)
          (magstep font-size)))
      (make-vcenter-markup
       (number->string rational)))))

  (define (format-small-fraction-vert rational)
    ;; Assume that the normal time-signature digit occupies two staff spaces.
    ;; Center the numerator in the upper space and the denominator in the lower
    ;; space.
    (make-fontsize-markup
     (if (number? nested-fraction-relative-font-size)
         nested-fraction-relative-font-size
         -5.5)
     (make-left-align-markup
      (make-combine-markup
       (make-translate-markup
        (cons
         0 ; X
         (* 3/2 ; Y
            (ly:output-def-lookup layout 'staff-space)
            (magstep font-size)))
        (make-center-align-markup
         (make-vcenter-markup
          (number->string (numerator rational)))))
       (make-translate-markup
        (cons
         0 ; X
         (* 1/2 ; Y
            (ly:output-def-lookup layout 'staff-space)
            (magstep font-size)))
        (make-center-align-markup
         (make-vcenter-markup
          (number->string (denominator rational)))))))))

  (define (format-term num-den-sign n)
    ;; num-den-sign is 1 when called for a numerator term, -1 when called for a
    ;; denominator term, 0 when called for single-number style.
    (cond
     ((or (integer? n) (inexact? n))
      (number->string n))
     ((and (number? nested-fraction-relative-font-size)
           (>= nested-fraction-relative-font-size 0))
      ;; At full size, the fraction must be oriented horizontally, so the only
      ;; remaining degree of freedom is whether the fraction is mixed or common.
      (if nested-fraction-mixed
          (format-mixed-fraction-horiz n)
          (number->string n)))
     (else
      (let* ((ipart (truncate n))
             (fpart (- n ipart)))
        (cond
         ((or (zero? ipart) (not nested-fraction-mixed)) ; as vulgar fraction
          (cond
           ((eq? nested-fraction-orientation 'horizontal)
            (if (and (number? nested-fraction-relative-font-size)
                     (< nested-fraction-relative-font-size 0))
                (format-small-fraction-horiz
                 n
                 ;; shift into the space toward the center of the staff
                 (- num-den-sign))
                (number->string n)))
           ((eq? nested-fraction-orientation 'vertical)
            (format-small-fraction-vert n))
           (else
            (if (and (number? nested-fraction-relative-font-size)
                     (< nested-fraction-relative-font-size 0))
                (format-small-fraction-vert n)
                (number->string n)))))
         ((eq? nested-fraction-orientation 'horizontal)
          (make-concat-markup
           (list
            (number->string ipart)
            (format-small-fraction-horiz fpart 1))))
         (else
          (make-concat-markup
           (list
            (number->string ipart)
            (format-small-fraction-vert fpart)))))))))

  (define (format-numerator-term n) (format-term 1 n))
  (define (format-denominator-term n) (format-term -1 n))
  (define (format-center-term n) (format-term 0 n))

  (define (format-time-fraction time-sig-fraction)
    (let* ((revargs (reverse time-sig-fraction))
           (den (car revargs))
           (nums (reverse (cdr revargs)))
           (nums-markup (make-line-markup
                         (insert-markups (map format-numerator-term nums) "+")))
           (note-dur (and (eq? denominator-style 'note)
                          (positive? den)
                          (ly:number->duration (/ den)))))
      (cond
       ((and note-dur (= 1 (ly:duration-scale note-dur)))
        (let ((only-note-stencil (interpret-markup
                                  layout props
                                  (make-note-by-number-markup
                                   (ly:duration-log note-dur) 0 DOWN)))
              (full-note-markup (make-note-by-number-markup
                                 (ly:duration-log note-dur)
                                 (ly:duration-dot-count note-dur) DOWN)))
          ;; After aligning the number and note, move the reference point back
          ;; to the left.
          (make-left-align-markup
           (make-combine-markup
            (make-center-align-markup nums-markup)
            (make-translate-markup
             (cons
              ;; X: center on note (without dots)
              (- (interval-center (ly:stencil-extent only-note-stencil X)))
              ;; Y: move to note-staff-position
              (* (ly:output-def-lookup layout 'staff-space)
                 (magstep font-size)
                 (/ note-staff-position 2)))
             (make-override-markup
              (list (cons 'dots-direction note-dots-direction)
                    (cons 'flag-style note-flag-style)
                    (cons 'style note-head-style))
              full-note-markup))))))
       ((eq? denominator-style 'none)
        (make-vcenter-markup nums-markup))
       (else
        ;; After centering the terms, move the reference point back to the left.
        (make-left-align-markup
         ;; make-center-column-markup allows slashes in rational values to push
         ;; the denominator down.  Overriding baseline-skip doesn't work to
         ;; reduce the spacing, so we combine the numerator and denominator the
         ;; long way.
         (make-combine-markup
          (make-center-align-markup nums-markup)
          (make-center-align-markup
           (make-translate-markup
            (cons 0 (* (ly:output-def-lookup layout 'staff-space)
                       (magstep font-size)
                       -2))
            (format-denominator-term den)))))))))

  (define (format-time-element time-sig)
    (cond ((number-pair? time-sig)
           (format-time-fraction (list (car time-sig) (cdr time-sig))))
          ((pair? (cdr time-sig))
           (format-time-fraction time-sig))
          (else
           (make-vcenter-markup (format-center-term (car time-sig))))))

  (define (format-time-list time-sig)
    (make-override-markup '(baseline-skip . 0)
                          (make-line-markup
                           (insert-markups (map format-time-element time-sig)
                                           (make-vcenter-markup "+")))))

  (define (format-compound-time time-sig)
    (make-number-markup
     (cond
      ((number? time-sig) (format-time-element (list time-sig)))
      ((number-pair? time-sig)
       (format-time-element (list (car time-sig) (cdr time-sig))))
      ((pair? (car time-sig)) (format-time-list time-sig))
      (else (format-time-element time-sig)))))

  (interpret-markup layout props (format-compound-time time-sig)))


;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Formatting of symbolic time signatures

(define-public (make-glyph-time-signature-markup style fraction)
  "Make markup for a symbolic time signature of the form
@code{timesig@/.@var{<style>}@/@var{<numerator>}@/@var{<denominator>}},
for example @samp{timesig.mensural34}.  If the music font does not
have a glyph for the requested style and fraction, issue a warning and
make a numbered time signature instead."
  (make-first-visible-markup
   (list (make-musicglyph-markup (string-append
                                  "timesig."
                                  (symbol->string style)
                                  (number->string (car fraction))
                                  (number->string (cdr fraction))))
         (make-compound-meter-markup fraction))))

(define-public (make-c-time-signature-markup fraction)
  "Make markup for the @q{C} time signature style."
  (let ((n (car fraction))
        (d (cdr fraction)))
    ;; check specific fractions to avoid warnings when no glyph exists
    (if (or (and (= n 2) (= d 2))
            (and (= n 4) (= d 4)))
        (make-glyph-time-signature-markup 'C fraction)
        (make-compound-meter-markup fraction))))


;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Time signature styles

(add-simple-time-signature-style 'numbered
                                 make-compound-meter-markup)
(add-simple-time-signature-style 'single-number
                                 (lambda (fraction)
                                   (make-compound-meter-markup
                                    (car fraction))))
(add-simple-time-signature-style 'C
                                 make-c-time-signature-markup)
(add-simple-time-signature-style 'default
                                 make-c-time-signature-markup)


;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Measure length calculation of (possibly complex) compound time signatures

(define (calculate-time-fraction time-sig-fraction)
  (let* ((revargs (reverse time-sig-fraction))
         (den (car revargs))
         (num (apply + (cdr revargs))))
    (/ num den)))

(define (calculate-complex-compound-time time-sig)
  (let accumulate ((sum 0)
                   (remaining (map calculate-time-fraction time-sig)))
    (if (pair? remaining)
        (accumulate (+ sum (car remaining)) (cdr remaining))
        sum)))

(define-public (calculate-compound-measure-length time-sig)
  (cond
   ((not (pair? time-sig)) 4/4)
   ((pair? (car time-sig)) (calculate-complex-compound-time time-sig))
   (else (calculate-time-fraction time-sig))))

;; TODO: print a deprecation warning when this is called (once per process)
(define-public (calculate-compound-measure-length-as-moment time-sig)
  (ly:make-moment (calculate-compound-measure-length time-sig)))


;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Beat base: Use the largest denominator (corresponding to the shortest
;;; duration) of all the fractions.
;;;
;;; Using lcm instead of max would yield a beat structure holding only integers,
;;; which is intuitively mathematically nice; however, we need to support
;;; non-integers in beatStructure anyway for fractional time signatures, and it
;;; is also intuitively nice for beatBase to come directly from the user.

(define (calculate-compound-beat-base-full time-sig)
  (apply max (map last time-sig)))

(define-public (calculate-compound-beat-base time-sig)
  (/
   (cond
    ((not (pair? time-sig)) 4)
    ((pair? (car time-sig)) (calculate-compound-beat-base-full time-sig))
    (else (calculate-compound-beat-base-full (list time-sig))))))

;; TODO: print a deprecation warning when this is called (once per process)
(define-public (calculate-compound-beat-base-as-moment time-sig)
  (ly:make-moment (calculate-compound-beat-base time-sig)))

;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Beat Grouping

(define (normalize-fraction frac beat)
  (let* ((thisbeat (car (reverse frac)))
         (factor (/ beat thisbeat)))
    (map (lambda (f) (* factor f)) frac)))

(define (beat-grouping-internal time-sig)
  ;; Normalize to given beat, extract the beats and join them to one list
  (let* ((beat (calculate-compound-beat-base-full time-sig))
         (normalized (map (lambda (f) (normalize-fraction f beat)) time-sig))
         (beats (map (lambda (f) (drop-right f 1)) normalized)))
    (concatenate beats)))

(define-public (calculate-compound-beat-grouping time-sig)
  (cond
   ((not (pair? time-sig)) '(2 . 2))
   ((pair? (car time-sig)) (beat-grouping-internal time-sig))
   (else (beat-grouping-internal (list time-sig)))))
