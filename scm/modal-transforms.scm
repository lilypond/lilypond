;;; modal-transforms.scm --- Modal transposition, inversion, and retrograde.

;; Copyright (C) 2011--2022 Ellis & Grant, Inc.

;; Author: Michael Ellis <michael.f.ellis@gmail.com>

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.


(define (transposer-factory scale)
  "Returns a transposer for the specified @var{scale}.
It is an error if either argument to a transposer is not in the scale
it was created with.  A transposer knows nothing about LilyPond
internals.  It treats scales as an ordered list of arbitrary items and
pitches as members of a scale.
"

  (define (index item lis)
    (list-index (lambda (x) (equal? item x)) lis))

  (lambda (from-pitch to-pitch pitch)
    (cond
     ((not (member from-pitch scale))
      (ly:warning (_i "'from' pitch not in scale; ignoring"))
      pitch)

     ((not (member to-pitch scale))
      (ly:warning (_i "'to' pitch not in scale; ignoring"))
      pitch)

     ((not (member pitch scale))
      (ly:warning (_i "pitch to be transposed not in scale; ignoring"))
      pitch)

     (else
      (list-ref scale
                (modulo
                 (+ (index pitch scale)
                    (- (index to-pitch scale)
                       (index from-pitch scale)))
                 (length scale)))))))

(define (inverter-factory scale)
  "Returns an inverter for the specified @var{scale}.
It is an error if either argument to an inverter
is not in the scale it was created with.  An inverter knows nothing
about LilyPond internals.  It treats scales as an ordered list of
arbitrary items and pitches as members of a scale.
"

  (define (index item lis)
    (list-index (lambda (x) (equal? item x)) lis))

  (lambda (around-pitch to-pitch pitch)
    (cond
     ((not (member around-pitch scale))
      (ly:warning (_i "'around' pitch not in scale; ignoring"))
      pitch)

     ((not (member to-pitch scale))
      (ly:warning (_i "'to' pitch not in scale; ignoring"))
      pitch)

     ((not (member pitch scale))
      (ly:warning (_i "pitch to be inverted not in scale; ignoring"))
      pitch)

     (else
      (list-ref scale
                (modulo
                 (+ (index to-pitch scale)
                    (- (index around-pitch scale)
                       (index pitch scale)))
                 (length scale)))))))

(define (replicate-modify lis n mod-proc)
  "Apply @code{(mod-proc lis n)} to each element of a list and
concatenate the results.  Knows nothing of LilyPond internals."
  (cond
   ((< n 0)
    (ly:warning (_i "negative replication count; ignoring")))
   ((= n 0)
    '())
   ((= n 1)
    (mod-proc lis 1))
   ((> n 1)
    (append
     (replicate-modify lis (- n 1) mod-proc)
     (mod-proc lis n)))))



(define-public (change-pitches music converter)
  "Recurse through @var{music}, applying @var{converter} to pitches.
@var{converter} is typically a transposer or an inverter (see file
@file{scm/@/modal-transforms.scm}), but may be user-defined.  The converter
function must take a single pitch as its argument and return a new pitch.
These are LilyPond Scheme pitches, e.g., @code{(ly:make-pitch 0 2 0)}.
"
  (let ((elements (ly:music-property music 'elements))
        (element (ly:music-property music 'element))
        (pitch (ly:music-property music 'pitch)))

    (cond
     ((ly:pitch? pitch)
      (ly:music-set-property! music 'pitch (converter pitch)))

     ((pair? elements)
      (for-each (lambda (x) (change-pitches x converter)) elements))

     ((ly:music? element)
      (change-pitches element converter)))))


(define (make-scale music)
  "Recurse through @var{music}, extracting pitches.
Returns a list of pitch objects, e.g
@code{'((ly:make-pitch 0 2 0) (ly:make-pitch 0 4 0) ... )}
Typically used to construct a scale for input to
@code{transposer-factory}."

  (let ((elements (ly:music-property music 'elements))
        (element (ly:music-property music 'element))
        (pitch (ly:music-property music 'pitch)))

    (cond
     ((ly:pitch? pitch)
      (list pitch))

     ((pair? elements)
      (append-map make-scale elements))

     ((ly:music? element)
      (make-scale element)))))

(define (make-extended-scale music)
  "Extend scale given by @var{music} by 5 octaves up and down."
  ;; This is a bit of a hack since, in theory, someone might want to
  ;; transpose further than 5 octaves from the original scale
  ;; definition.  In practice this seems unlikely to occur very often.
  (define extender
    (lambda (lis n)
      (map
       (lambda (i)
         (ly:make-pitch
          (+ (- n 6) (ly:pitch-octave i))
          (ly:pitch-notename i)
          (ly:pitch-alteration i)))
       lis)))

  (let ((scale (make-scale music)))
    (replicate-modify scale 11 extender)))


;; ------------- PUBLIC FUNCTIONS -----------------------------

(define-public (make-modal-transposer from to scale)
  "Wrapper function for @code{transposer-factory}."
  (let ((transposer (transposer-factory (make-extended-scale scale))))
    (lambda (p)
      (transposer from to p))))

(define-public (make-modal-inverter around to scale)
  "Wrapper function for @code{inverter-factory}."
  (let ((inverter (inverter-factory (make-extended-scale scale))))
    (lambda (p)
      (inverter around to p))))


(define-public (retrograde-music music)
  "Return @var{music} in retrograde (reversed) order."
  ;; Included here to allow this module to provide a complete set of
  ;; common formal operations on motives, i.e transposition,
  ;; inversion and retrograding.

  (define (reverse-span! m)
    ;; invert direction of two-sided spanners
    (let ((spd (ly:music-property m 'span-direction)))
      (if (ly:dir? spd)
          (begin
            (set! (ly:music-property m 'span-direction) (- spd))
            (case (ly:music-property m 'name)
              ((CrescendoEvent)
               (make-music 'DecrescendoEvent m))
              ((DecrescendoEvent)
               (make-music 'CrescendoEvent m))
              (else m)))
          m)))

  ;; carryover is a possible list of tie events, the loop returns any
  ;; such trailing list from the given expression
  (define (loop m carryover)
    (define (filter-ties! m carryover field)
      (let ((vals (ly:music-property m field)))
        (if (pair? vals)
            (call-with-values
                (lambda ()
                  (partition! (music-type-predicate
                               '(tie-event glissando-event)) vals))
              (lambda (ties no-ties)
                (set! (ly:music-property m field)
                      (append! (map! reverse-span! no-ties) carryover))
                ties))
            (begin
              (if (pair? carryover)
                  (set! (ly:music-property m field) carryover))
              '()))))

    ;; The reversal will let some prefatory material stay in front of
    ;; the following element.  Most prominently single
    ;; overrides/reverts/sets/unsets and applyContext.  This does not
    ;; change the position of a clef (which will generally be useless
    ;; after retrograding) but it does not jumble the clef change
    ;; command internals.  Also, stuff like \once\override stays at
    ;; the affected element.

    (define (prefatory? m)
      (or ((music-type-predicate
            '(apply-context apply-output-event layout-instruction-event)) m)
          (and
           (music-is-of-type? m 'music-wrapper-music)
           (prefatory? (ly:music-property m 'element)))))

    (define (musiclistreverse lst)
      (let loop ((lst lst) (res '()) (zeros '()))
        (cond ((null? lst) (reverse! zeros res))
              ((prefatory? (car lst))
               (loop (cdr lst) res (cons (car lst) zeros)))
              (else
               (loop (cdr lst) (reverse! zeros (cons (car lst) res)) '())))))

    (cond ((music-is-of-type? m 'event-chord)
           (let* ((chord-ties
                   (append!
                    (filter-ties! m carryover 'elements)
                    ;; articulations on an event-chord do not occur
                    ;; "naturally" but are supported when user-generated
                    ;; elsewhere, so we treat them properly
                    (filter-ties! m '() 'articulations)))
                  ;; in-chord ties are converted to per-chord ties.
                  ;; This is less than optimal but pretty much the
                  ;; best we can hope to achieve with this approach.
                  (element-ties
                   (append-map!
                    (lambda (m) (filter-ties! m '() 'articulations))
                    (ly:music-property m 'elements))))
             (append! chord-ties element-ties)))

          ((music-is-of-type? m 'rhythmic-event)
           (filter-ties! m carryover 'articulations))

          ;; The following is hardly correct but tieing inside of
          ;; <<...>> is really beyond our pay grade.
          ((music-is-of-type? m 'simultaneous-music)
           (append-map! (lambda (m) (loop m (ly:music-deep-copy carryover)))
                        (ly:music-property m 'elements)))
          (else
           (let ((elt (ly:music-property m 'element))
                 (elts (ly:music-property m 'elements)))
             (let ((res
                    (fold loop
                          (if (ly:music? elt) (loop elt carryover) carryover)
                          elts)))
               (if (ly:music? elt)
                   (set! (ly:music-property m 'element)
                         (reverse-span! elt)))
               (if (pair? elts)
                   (set! (ly:music-property m 'elements)
                         (map! reverse-span! (musiclistreverse elts))))
               (append! res (filter-ties! m '() 'articulations)))))))
  (let ((dangling (loop music '())))
    (for-each
     (lambda (t) (ly:music-warning t (G_ "Dangling tie in \\retrograde")))
     dangling))
  music)

(define-public (pitch-invert around to music)
  (_i "If @var{music} is a single pitch, inverts it about @var{around}
and transposes from @var{around} to @var{to}.")
  (let ((p (ly:music-property music 'pitch)))
    (if (ly:pitch? p)
        (ly:music-set-property!
         music 'pitch
         (ly:pitch-transpose to (ly:pitch-diff around p))))
    music))

(define-public (music-invert around to music)
  (_i "Applies pitch-invert to all pitches in @var{music}.")
  (music-map (lambda (x) (pitch-invert around to x)) music))


;; ---------- Transform chord voicings ------------------------

(define-public (move-chord-note n direction)
  (_i "Transpose a note (numbered as @var{n}) in @var{direction}.
@var{n} is zero-based and can be negative to count from the end.")
  (lambda (music)
    (if (music-is-of-type? music 'event-chord)
        (let* ((elts (extract-typed-music music 'note-event))
               (l (length elts))
               ;; if direction is up, count from the bottom note upward,
               ;; if direction is down, count from the top note downward.
               (count-from (if (negative? n) (+ l n) n))
               ;; Notes may not have been entered from bottom to top;
               ;; sort them depending on their pitch.
               (notes (sort-list elts
                                 (lambda (a b)
                                   (ly:pitch<?
                                    (ly:music-property a 'pitch)
                                    (ly:music-property b 'pitch))))))
          (if (< -1 count-from l)
              (let* ((note (list-ref notes count-from))
                     (oct (ly:music-property note 'octavation 0))
                     (chord-limit (car (if (negative? direction)
                                           notes (reverse notes))))
                     (octs (ly:pitch-octave
                            (ly:pitch-diff
                             (ly:music-property chord-limit 'pitch)
                             (ly:music-property note 'pitch)))))
                (if (positive? direction) (set! octs (1+ octs)))
                (ly:music-transpose note (ly:make-pitch octs 0))
                (set! (ly:music-property note 'octavation) (+ oct octs))))))
    music))
