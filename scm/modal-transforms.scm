;;; modal-transforms.scm --- Modal transposition, inversion, and retrograde.

;; Copyright (C) 2011--2012 Ellis & Grant, Inc.

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
Converter is typically a transposer or an inverter as defined above in
this module, but may be user-defined.  The converter function must take
a single pitch as its argument and return a new pitch.  These are
LilyPond scheme pitches, e.g. @code{(ly:make-pitch 0 2 0)}
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
Typically used to construct a scale for input to transposer-factory
(see).
"

  (let ((elements (ly:music-property music 'elements))
        (element (ly:music-property music 'element))
        (pitch (ly:music-property music 'pitch)))

    (cond
     ((ly:pitch? pitch)
      (list pitch))

     ((pair? elements)
      (append-map
       (lambda (x) (make-scale x))
       elements))

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
  "Wrapper function for transposer-factory."
  (let ((transposer (transposer-factory (make-extended-scale scale))))
    (lambda (p)
      (transposer from to p))))

(define-public (make-modal-inverter around to scale)
  "Wrapper function for inverter-factory"
  (let ((inverter (inverter-factory (make-extended-scale scale))))
    (lambda (p)
      (inverter around to p))))


(define-public (retrograde-music music)
  "Returns @var{music} in retrograde (reversed) order."
  ;; Copied from LSR #105 and renamed.
  ;; Included here to allow this module to provide a complete set of
  ;; common formal operations on motives, i.e transposition,
  ;; inversion and retrograding.

  (let* ((elements (ly:music-property music 'elements))
         (reversed (reverse elements))
         (element (ly:music-property music 'element))
         (span-dir (ly:music-property music 'span-direction)))

    (ly:music-set-property! music 'elements reversed)

    (if (ly:music? element)
        (ly:music-set-property!
         music 'element
         (retrograde-music element)))

    (if (ly:dir? span-dir)
        (ly:music-set-property! music 'span-direction (- span-dir)))

    (for-each retrograde-music reversed)

    music))

(define-public (pitch-invert around to music)
  "If @var{music} is a single pitch, inverts it about @var{around}
and transposes from @var{around} to @var{to}."
  (let ((p (ly:music-property music 'pitch)))
    (if (ly:pitch? p)
        (ly:music-set-property!
         music 'pitch
         (ly:pitch-transpose to (ly:pitch-diff around p))))
    music))

(define-public (music-invert around to music)
  "Applies pitch-invert to all pitches in @var{music}."
  (music-map (lambda (x) (pitch-invert around to x)) music))
