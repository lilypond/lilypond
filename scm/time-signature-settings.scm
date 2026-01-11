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
;;;   (submeasureStructure . structure-list)
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

    ;; in 2/4 time:
    ;;   use defaults -- no entries necessary

    ;; in 2/8  time:
    ;;   beam entire measure together
    ((2 . 8) . ((beamExceptions . ((end . ((1/8 . (2))))))))

    ;; in 2/16 time:
    ;;   use defaults -- no entries necessary

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

(define (time-signature->list time-sig)
  "Convert canonical time signature @var{time-sig} to a list of fractions: if
@var{time-sig} is a single fraction, wrap it in a list; otherwise, pass
@var{time-sig} through."
  (if (number? (cdr time-sig))
      (list time-sig)
      time-sig))

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
See the @code{\\time} command for a description of canonical form.

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

(define-public (time-signature->fraction time-sig)
  "Reduce canonical time signature @var{time-sig} to a single, simple fraction
(or #f)."
  (if (or (not time-sig) (number-pair? time-sig))
      ;; already a single simple fraction or #f
      time-sig
      ;; lump the numerator terms together over the beat base
      (let* ((settings '())
             (base (beat-base time-sig settings))
             (structure (beat-structure base time-sig settings)))
        (cons (apply + structure) (/ base)))))

(define (get-setting my-symbol time-signature time-signature-settings)
  "Get setting @code{my-symbol} for @code{time-signature} from
@code{time-signature-settings}."
  (let ((my-time-signature-settings
         (assoc-get time-signature time-signature-settings '())))
    (assoc-get my-symbol my-time-signature-settings '())))

(define-public (make-setting beat-base
                             structure
                             beam-exceptions)
  (let* ((my-beat-structure
          (optionally-grouped-beat-structure->beat-structure structure))
         (my-submeasure-structure
          (optionally-grouped-beat-structure->submeasure-structure structure))
         (result (list
                  (cons 'beatBase (musical-length->number beat-base))
                  (cons 'beamExceptions beam-exceptions))))
    (when (not (null? my-submeasure-structure))
      (set! result (acons 'submeasureStructure my-submeasure-structure result)))
    (when (not (null? my-beat-structure))
      (set! result (acons 'beatStructure my-beat-structure result)))
    result))

(define-public (calc-measure-length time-sig)
  "Calculate the measure length for @var{time-sig}.

@var{time-sig} must be a sane, canonical time signature."

  (define (fraction->length time-sig-fraction)
    (let* ((num (car time-sig-fraction))
           (den (cdr time-sig-fraction))
           (num-sum (if (number? num) num (apply + num))))
      (if (zero? den) ; avoid integer div error
          +inf.0
          (/ num-sum den))))

  (if (pair? time-sig)
      (apply + (map fraction->length (time-signature->list time-sig)))
      +inf.0)) ; time-sig was #f for senza misura (or else garbage)

(define-public (beat-base time-sig time-signature-settings)
  "Get the @code{beatBase} value for @var{time-sig} from @var{time-signature-settings}.

@var{time-sig} must be a sane, canonical time signature.

If there is no entry, derive a value from @var{time-sig}."

  (define (least-element lst)
    "The built-in min converts exact numbers to inexact if +inf.0 appears in the
list.  This does not."
    (define (lesser a b) (if (< a b) a b))
    (fold lesser (car lst) (cdr lst)))

  (let ((from-table (get-setting 'beatBase time-sig time-signature-settings)))
    (cond
     ((not (null? from-table))
      from-table)
     ((not (pair? time-sig)) ; #f for senza misura (or else garbage)
      +inf.0)
     ((not (number? (cdr time-sig))) ; two or more fractions
      ;; Use the shortest base of the component fractions.  Using the lcm of the
      ;; denominators would yield a beat structure holding only integers, which
      ;; is intuitively mathematically nice; however, we need to support
      ;; non-integers in beatStructure anyway for fractional time signatures,
      ;; and it is also intuitively nice for beatBase to come directly from the
      ;; user.
      (least-element (map (lambda (ts)
                            (beat-base ts time-signature-settings))
                          time-sig)))
     (else ; a single fraction (simple or subdivided)
      (let ((den (cdr time-sig)))
        (if (zero? den) ; avoid integer div error
            +inf.0
            (/ den)))))))

(define-public (optionally-grouped-beat-structure->beat-structure structure)
  "If @var{structure} has groups defining submeasures, ungroup them."
  (cond
   ((number-list? structure) ; including '()
    structure)
   ((grouped-number-list? structure)
    (apply append structure)) ; ungroup
   (else ; Would logging an error be helpful?
    '())))

(define-public (optionally-grouped-beat-structure->submeasure-structure
                structure)
  "If the beats in @var{structure} are grouped, extract the submeasure structure from them."
  (cond
   ((number-list? structure) ; including '()
    '()) ; no submeasure structure specified
   ((grouped-number-list? structure)
    (map (lambda (group)
           (apply + group))
         structure))
   (else ; Would logging an error be helpful?
    '())))

(define-public (beat-structure base time-sig time-signature-settings)
  "Get the @code{beatStructure} value for @var{time-sig} from @var{time-signature-settings}, scaled to @var{base} units.

@var{time-sig} must be a sane, canonical time signature.

If there is no entry, derive a structure from @var{time-sig}."

  ;; Look up the beat structure that would be used for the given fraction in
  ;; isolation.  Then, scale it to the beat base that will be used for the
  ;; concatenated time signature.
  (define (recur time-sig-fraction)
    (let* ((lone-base (beat-base time-sig-fraction time-signature-settings))
           (lone-struct (beat-structure lone-base time-sig-fraction
                                        time-signature-settings))
           (beat-factor (/ base lone-base)))
      (if (zero? beat-factor)
          '(+inf.0) ; unexpected in this context
          (map (lambda (x) (/ x beat-factor)) lone-struct))))

  ;; Subdivide a simple fraction using the convention that a numerator that is a
  ;; multiple of 3 (>=6) indicates triplet subdivision.
  (define (calc-simple-fraction-structure time-sig-fraction)
    (let* ((num (car time-sig-fraction))
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

  ;; Return numerator terms, which define the beat structure.
  (define (calc-subdivided-fraction-structure time-sig-fraction)
    (ensure-list (car time-sig-fraction)))

  (if (not (finite? base))
      '() ; senza misura: scaling the structure (if any) is meaningless
      (let* ((settings (assoc-get time-sig time-signature-settings '()))
             (table-struct (assoc-get 'beatStructure settings '())))
        (if (and (null? table-struct)
                 (pair? time-sig)
                 (not (number? (cdr time-sig))))
            ;; time-sig is two or more fractions, representing a strictly
            ;; alternating time signature.  Also, it has no entry in the
            ;; settings. (Finding an entry in the settings would be unusual for
            ;; this kind of time signature, but we allow it.)
            (concatenate (map recur time-sig))
            (let* ((table-base (assoc-get 'beatBase settings '()))
                   (beat-factor
                    (/ base
                       (if (null? table-base)
                           (beat-base time-sig time-signature-settings)
                           table-base)))
                   (struct
                    (cond
                     ((not (null? table-struct))
                      table-struct)
                     ((not (pair? time-sig)) ; #f for senza misura (or garbage)
                      '())
                     ((number? (car time-sig))
                      (calc-simple-fraction-structure time-sig))
                     (else
                      (calc-subdivided-fraction-structure time-sig)))))
              (if (zero? beat-factor)
                  ;; Likely: The computed beat base is +inf.0 for senza misura.
                  ;; Unexpected: The beat base found in the settings is +inf.0
                  ;; or the beat base function argument is zero.
                  '(+inf.0)
                  (map (lambda (x) (/ x beat-factor)) struct)))))))

(define-public (calc-submeasure-structure base time-sig time-signature-settings)
  "Get the @code{submeasureStructure} value for @var{time-sig}.

Look up the value in @var{time-signature-settings} and scale it to @var{base}
units.  If there is no entry, derive a structure from @var{time-sig}.

@var{time-sig} must be a sane, canonical time signature.
"

  ;; Look up the measure structure that would be used for the given fraction in
  ;; isolation.  Then, scale it to the beat base that will be used for the
  ;; concatenated time signature.
  (define (recur time-sig-fraction)
    (let* ((lone-base (beat-base time-sig-fraction time-signature-settings))
           (lone-struct (calc-submeasure-structure lone-base time-sig-fraction
                                                   time-signature-settings))
           (beat-factor (/ base lone-base)))
      (if (zero? beat-factor)
          '(+inf.0) ; unexpected in this context
          (map (lambda (x) (/ x beat-factor)) lone-struct))))

  ;; By default, we don't subdivide single fractions (whether with a simple or
  ;; subdivided numerator).  Return a one-element list with an entry covering
  ;; the full measure length.  This allows concatenating several of these lists
  ;; to generate a submeasure structure for a strictly alternating time
  ;; signature.
  (define (calc-single-fraction-structure time-sig-fraction)
    (let* ((measure-length (calc-measure-length time-sig-fraction))
           (den (cdr time-sig-fraction))
           (measure-beats (* measure-length den)))
      (if (zero? measure-beats)
          '()
          (list measure-beats))))

  (if (not (finite? base))
      '() ; senza misura: scaling the structure (if any) is meaningless
      (let* ((settings (assoc-get time-sig time-signature-settings '()))
             (table-struct (assoc-get 'submeasureStructure settings '())))
        (if (and (null? table-struct)
                 (pair? time-sig)
                 (not (number? (cdr time-sig))))
            ;; time-sig is two or more fractions, representing a strictly
            ;; alternating time signature.  Also, it has no entry in the
            ;; settings. (Finding an entry in the settings would be unusual for
            ;; this kind of time signature, but we allow it.)
            (concatenate (map recur time-sig))
            (let* ((table-base (assoc-get 'beatBase settings '()))
                   (beat-factor
                    (/ base
                       (if (null? table-base)
                           (beat-base time-sig time-signature-settings)
                           table-base)))
                   (struct
                    (cond
                     ((not (null? table-struct))
                      table-struct)
                     ((not (pair? time-sig)) ; #f for senza misura (or garbage)
                      '())
                     ((number? (car time-sig))
                      (calc-single-fraction-structure time-sig))
                     (else
                      (calc-single-fraction-structure time-sig)))))
              (if (zero? beat-factor)
                  ;; Likely: The computed beat base is +inf.0 for senza misura.
                  ;; Unexpected: The beat base found in the settings is +inf.0
                  ;; or the beat base function argument is zero.
                  '(+inf.0)
                  (map (lambda (x) (/ x beat-factor)) struct)))))))

(define-public (beam-exceptions time-sig time-signature-settings)
  "Get the @code{beamExceptions} value for @var{time-sig} from @var{time-signature-settings}.

@var{time-sig} must be a sane, canonical time signature."

  (define (get-grouping-lists-for-beam-type
           ;; a sequence of alists of (beam-type . grouping-list)
           component-exceptions
           ;; the beam type of interest
           beam-type
           ;; a sequence of grouping lists for the next longest beam type
           ;; (same length as component-exceptions)
           prev-grouping-lists
           ;; the next longest beam type
           prev-beam-type)
    "Map component-exceptions to a list of its grouping lists for beam-type.
Whenever an element of component-exceptions has no entry for beam-type, default
to the corresponding element of prev-grouping-lists, rescaled from
prev-beam-type to beam-type."
    (map (lambda (defined-exceptions default-grouping-list)
           (let ((gl (assoc-get beam-type defined-exceptions '())))
             (if (null? gl)
                 (let ((factor (/ prev-beam-type beam-type)))
                   (map (lambda (x) (* x factor)) default-grouping-list))
                 gl)))
         component-exceptions prev-grouping-lists))

  (define (compose-end-exceptions
           ;; a sequence of alists of (beam-type . grouping-list)
           component-exceptions
           ;; union of beam types from all exceptions, sorted longest duration
           ;; first
           beam-types)
    ;; returns an alist of (beam-type . grouping-list)
    (let* ((bt-prev 1) ; previously handled beam type: initially arbitrary
           ;; grouping lists for the previously handled beam type: initially,
           ;; the component beat structures
           (gls-prev
            (map (lambda (ts)
                   (beat-structure bt-prev ts time-signature-settings))
                 time-sig)))
      (map (lambda (beam-type)
             (let ((gls (get-grouping-lists-for-beam-type
                         component-exceptions
                         beam-type
                         gls-prev
                         bt-prev)))
               (set! bt-prev beam-type)
               (set! gls-prev gls)
               (cons beam-type (concatenate gls))))
           beam-types)))
  
  (define (compose-beaming-exceptions)
    "Compose beam exceptions for a strictly alternating time signature."
    (let* (;; 'end exceptions for each component of the alternating time sig.
           (component-end-exceptions
            (map (lambda (ts)
                   (let ((x (beam-exceptions ts time-signature-settings)))
                     (assoc-get 'end
                                x
                                '())))
                 time-sig))
           ;; The union of beam types found among component exceptions, sorted
           ;; longest duration first.  The composite exceptions will have an
           ;; entry for each.
           (beam-types (uniq-list (sort
                                   (concatenate
                                    (map alist-keys component-end-exceptions))
                                    >))))
      (if (null? beam-types)
          '()
          (list (cons 'end (compose-end-exceptions
                            component-end-exceptions
                            beam-types))))))

  (let ((from-table (get-setting 'beamExceptions time-sig
                                 time-signature-settings)))
    (cond
     ((not (null? from-table))
      from-table)
     ((not (pair? time-sig)) ; #f for senza misura (or else garbage)
      '())
     ((not (number? (cdr time-sig))) ; two or more fractions
      (compose-beaming-exceptions))
     (else ; a single fraction
      '()))))


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
;;; Formatting of complex time signatures

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

  (define (format-time-fraction canonical)
    (let* ((den (cdr canonical))
           (nums (if (pair? (car canonical))
                     (car canonical)
                     (list (car canonical))))
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

  (define (format-time-fraction-abbr abbreviated)
    (format-time-fraction (tsig-fraction-abbr-expand abbreviated)))

  (define (format-time-element time-sig)
    (cond ((number? (cdr time-sig)) ; canonical fraction
           (format-time-fraction time-sig))
          ((pair? (cdr time-sig)) ; abbreviated fraction
           (format-time-fraction-abbr time-sig))
          (else ; list of one element
           (make-vcenter-markup (format-center-term (car time-sig))))))

  (define (format-time-list time-sig)
    (make-override-markup '(baseline-skip . 0)
                          (make-line-markup
                           (insert-markups (map format-time-element time-sig)
                                           (make-vcenter-markup "+")))))

  (define (format-compound-time time-sig)
    (make-number-markup
     (cond
      ((number? time-sig)
       (format-time-element (list time-sig)))
      ((number? (cdr time-sig)) ; canonical fraction
       (format-time-fraction time-sig))
      ((pair? (car time-sig)) ; list of (maybe abbreviated) fractions
       (format-time-list time-sig))
      (else ; abbreviated fraction
       (format-time-element time-sig)))))

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

(define-public (make-c-time-signature-markup spec)
  "Make markup for the @q{C} time signature style."
  (if (number-pair? spec)
      (let ((n (car spec))
            (d (cdr spec)))
        ;; check specific fractions to avoid warnings when no glyph exists
        (if (or (and (= n 2) (= d 2))
                (and (= n 4) (= d 4)))
            (make-glyph-time-signature-markup 'C spec)
            (make-compound-meter-markup spec)))
      (make-compound-meter-markup spec)))


;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Time signature styles

(add-simple-time-signature-style 'numbered
                                 make-compound-meter-markup)
(add-simple-time-signature-style 'single-number
                                 (lambda (spec)
                                   (if (number-pair? spec)
                                       (make-override-markup
                                        '(denominator-style . none)
                                        (make-compound-meter-markup spec))
                                       (make-compound-meter-markup spec))))
(add-simple-time-signature-style 'C
                                 make-c-time-signature-markup)
(add-simple-time-signature-style 'default
                                 make-c-time-signature-markup)

;;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Legacy compound-meter functions
;;;
;;; TODO: Print a deprecation warning when any of these is called (once per
;;; function per process, ideally).

(define-public (calculate-compound-measure-length time-sig-abbr)
  (calc-measure-length (tsig-abbr-expand time-sig-abbr)))

(define-public (calculate-compound-measure-length-as-moment time-sig-abbr)
  (ly:make-moment (calculate-compound-measure-length time-sig-abbr)))

(define-public (calculate-compound-beat-base time-sig-abbr)
  (beat-base (tsig-abbr-expand time-sig-abbr) '()))

(define-public (calculate-compound-beat-base-as-moment time-sig-abbr)
  (ly:make-moment (calculate-compound-beat-base time-sig-abbr)))

(define-public (calculate-compound-beat-grouping time-sig-abbr)
  (let* ((time-signature-settings '())
         (time-sig (tsig-abbr-expand time-sig-abbr))
         (base (beat-base time-sig time-signature-settings)))
    (beat-structure base time-sig time-signature-settings)))
