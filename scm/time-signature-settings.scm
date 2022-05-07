;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2009--2022 Carl Sorensen <c_sorensen@byu.edu>
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
;;;   (baseMoment . (/ numerator denominator))
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
;;;     the duration used is baseMoment.
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
;;; If no values are given for baseMoment and measureBeats, default values
;;;   will be assigned:
;;;   baseMoment gets the value (/ time-signature-denominator)
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

(define (get-setting my-symbol time-signature time-signature-settings)
  "Get setting @code{my-symbol} for @code{time-signature} from
@code{time-signature-settings}."
  (let ((my-time-signature-settings
         (assoc-get time-signature time-signature-settings '())))
    (assoc-get my-symbol my-time-signature-settings '())))

(define-public (make-setting base-fraction
                             beat-structure
                             beam-exceptions)
  (list
   (cons 'baseMoment (if (pair? base-fraction)
                         (/ (car base-fraction) (cdr base-fraction))
                         base-fraction))
   (cons 'beatStructure beat-structure)
   (cons 'beamExceptions beam-exceptions)))

(define-public (base-length time-signature time-signature-settings)
  "Get @code{baseMoment} rational value for @var{time-signature} from
@var{time-signature-settings}."
  (let ((return-value (get-setting 'baseMoment
                                   time-signature
                                   time-signature-settings)))
    (if (null? return-value)
        (/ (cdr time-signature))
        return-value)))

(define-public (beat-structure base-length time-signature time-signature-settings)
  "Get @code{beatStructure} value in @var{base-length} units
for @var{time-signature} from @var{time-signature-settings}."

  (let ((return-value (get-setting 'beatStructure
                                   time-signature
                                   time-signature-settings)))
    (if (null? return-value)
        ;; calculate default beatStructure
        (let* ((numerator (car time-signature))
               (group-size (if (and (> numerator 3)
                                    (zero? (remainder numerator 3)))
                               3
                               1))
               (beat-count (/ (car time-signature)
                              (cdr time-signature)
                              base-length
                              group-size)))
          (if (integer? beat-count)
              (make-list beat-count group-size)
              '()))
        ;; use value obtained from time-signature-settings
        return-value)))

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

;;; Use a centered-column inside a left-column, because the centered column
;;; moves its reference point to the center, which the left-column undoes.
(define (format-time-fraction time-sig-fraction)
  (let* ((revargs (reverse (map number->string time-sig-fraction)))
         (den (car revargs))
         (nums (reverse (cdr revargs))))
    (make-override-markup
     '(baseline-skip . 0)
     (make-left-column-markup
      (list (make-center-column-markup
             (list (make-line-markup (insert-markups nums "+"))
                   den)))))))

(define (format-time-numerator time-sig)
  (make-vcenter-markup (number->string (car time-sig))))

(define (format-time-element time-sig)
  (cond ((number-pair? time-sig)
         (format-time-fraction (list (car time-sig) (cdr time-sig))))
        ((pair? (cdr time-sig))
         (format-time-fraction time-sig))
        (else
         (format-time-numerator time-sig))))

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

(define-markup-command (compound-meter layout props time-sig)
  (number-or-pair?)
  #:category music
  "Draw a numeric time signature.

@lilypond[verbatim,quote]
\\markup {
  \\column {
    \\line { Single number:
               \\compound-meter #3 }
    \\line { Conventional:
               \\compound-meter #'(4 . 4) or
               \\compound-meter #'(4 4) }
    \\line { Compound:
               \\compound-meter #'(2 3 8) }
    \\line { Single-number compound:
               \\compound-meter #'((2) (3)) }
    \\line { Complex compound:
               \\compound-meter #'((2 3 8) (3 4)) }
  }
}
@end lilypond
"
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
(add-simple-time-signature-style 'single-digit
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
    (ly:make-moment num den)))

(define (calculate-complex-compound-time time-sig)
  (let add-moment ((moment ZERO-MOMENT)
                   (remaining (map calculate-time-fraction time-sig)))
    (if (pair? remaining)
        (add-moment (ly:moment-add moment (car remaining)) (cdr remaining))
        moment)))

(define-public (calculate-compound-measure-length time-sig)
  (cond
   ((not (pair? time-sig)) (ly:make-moment 4 4))
   ((pair? (car time-sig)) (calculate-complex-compound-time time-sig))
   (else (calculate-time-fraction time-sig))))


;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Base beat length: Use the smallest denominator from all fraction

(define (calculate-compound-base-beat-full time-sig)
  (apply max (map last time-sig)))

(define-public (calculate-compound-base-beat time-sig)
  (ly:make-moment
   1
   (cond
    ((not (pair? time-sig)) 4)
    ((pair? (car time-sig)) (calculate-compound-base-beat-full time-sig))
    (else (calculate-compound-base-beat-full (list time-sig))))))


;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Beat Grouping

(define (normalize-fraction frac beat)
  (let* ((thisbeat (car (reverse frac)))
         (factor (/ beat thisbeat)))
    (map (lambda (f) (* factor f)) frac)))

(define (beat-grouping-internal time-sig)
  ;; Normalize to given beat, extract the beats and join them to one list
  (let* ((beat (calculate-compound-base-beat-full time-sig))
         (normalized (map (lambda (f) (normalize-fraction f beat)) time-sig))
         (beats (map (lambda (f) (drop-right f 1)) normalized)))
    (concatenate beats)))

(define-public (calculate-compound-beat-grouping time-sig)
  (cond
   ((not (pair? time-sig)) '(2 . 2))
   ((pair? (car time-sig)) (beat-grouping-internal time-sig))
   (else (beat-grouping-internal (list time-sig)))))
