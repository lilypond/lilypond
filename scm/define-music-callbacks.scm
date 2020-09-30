;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;                 Neil Puttock <n.puttock@gmail.com>
;;;;                 Carl Sorensen <c_sorensen@byu.edu>
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

;; TODO: should link back into user manual.

(define (mm-rest-child-list music)
  "Generate events for multimeasure rests,
to be used by the sequential-iterator"
  (ly:set-origin! (list (make-music 'BarCheck)
                        (make-music 'MultiMeasureRestEvent
                                    (ly:music-deep-copy music))
                        (make-music 'BarCheck))
                  music))

(define (make-unfolded-set music)
  (let ((n (ly:music-property music 'repeat-count))
        (alts (ly:music-property music 'elements))
        (body (ly:music-property music 'element)))
    (cond ((<= n 0) '())
          ((null? alts) (make-list n body))
          (else
           (concatenate
            (zip (make-list n body)
                 (append! (make-list (max 0 (- n (length alts)))
                                     (car alts))
                          alts)))))))

(define (make-volta-set music)
  (let* ((alts (ly:music-property music 'elements))
         (lalts (length alts))
         (times (ly:music-property music 'repeat-count)))
    (map (lambda (x y)
           (make-music
            'SequentialMusic
            'elements
            ;; set properties for proper bar numbering
            (append
             (list (make-music 'AlternativeEvent
                               'alternative-dir (if (= y 0)
                                                    -1
                                                    0)
                               'alternative-increment
                               (if (= 0 y)
                                   (1+ (- times
                                          lalts))
                                   1)))
             (list x)
             (if (= y (1- lalts))
                 (list (make-music 'AlternativeEvent
                                   'alternative-dir 1
                                   'alternative-increment 0))
                 '()))))
         alts
         (iota lalts))))

(define (make-ottava-set music)
  "Set context properties for an ottava bracket."
  (let ((octavation (ly:music-property music 'ottava-number)))
    (list (context-spec-music
           (make-apply-context
            (lambda (context)
              (let* ((offset (* -7 octavation))
                     (markups (ly:context-property context 'ottavationMarkups))
                     (ottavation-markup (assoc-get octavation markups)))
                (set! (ly:context-property context 'middleCOffset) offset)
                (set! (ly:context-property context 'ottavation) ottavation-markup)
                ; For some cases it does not matter if the ottavation markup
                ; needed for the current octavation is missing.
                ; - if there is no octavation active
                ; - if 'ottavationMarkups is not defined (e.g. for performers
                ;   like in MIDI output)
                ; Output a warning only if none of these conditions are true
                (if (and (not (zero? octavation))
                         (not (null? markups))
                         (not (markup? ottavation-markup)))
                    (ly:warning (_ "Could not find ottavation markup for ~a octaves up.") octavation))
                (ly:set-middle-C! context))))
           'Staff))))

(define (make-time-signature-set music)
  "Set context properties for a time signature."
  (let* ((num (ly:music-property music 'numerator))
         (den (ly:music-property music 'denominator))
         (structure (ly:music-property music 'beat-structure))
         (fraction (cons num den)))
    (list (context-spec-music
           (make-apply-context
            (lambda (context)
              (let* ((time-signature-settings
                      (ly:context-property context 'timeSignatureSettings))
                     (my-base-length
                      (base-length fraction time-signature-settings))
                     (my-beat-structure
                      (if (null? structure)
                          (beat-structure my-base-length
                                          fraction
                                          time-signature-settings)
                          structure))
                     (beaming-exception
                      (beam-exceptions fraction time-signature-settings))
                     (new-measure-length (ly:make-moment num den)))
                (ly:context-set-property!
                 context 'timeSignatureFraction fraction)
                (ly:context-set-property!
                 context 'baseMoment (ly:make-moment my-base-length))
                (ly:context-set-property!
                 context 'beatStructure my-beat-structure)
                (ly:context-set-property!
                 context 'beamExceptions beaming-exception)
                (ly:context-set-property!
                 context 'measureLength new-measure-length))))
           'Timing)
          ;; (make-music 'TimeSignatureEvent music) would always
          ;; create a Bottom context.  So instead, we just send the
          ;; event to whatever context may be currently active.  If
          ;; that is not contained within an existing context with
          ;; TimeSignatureEngraver at the time \time is iterated, it
          ;; will drop through the floor which mostly means that
          ;; point&click and tweaks are not available for any time
          ;; signatures engraved due to the Timing property changes
          ;; but without a \time of its own.  This is more a
          ;; "notification" rather than an "event" (which is always
          ;; sent to Bottom) but we don't currently have iterators for
          ;; that.
          (descend-to-context
           (make-apply-context
            (lambda (context)
              (ly:broadcast (ly:context-event-source context)
                            (ly:make-stream-event
                             (ly:make-event-class 'time-signature-event)
                             (ly:music-mutable-properties music)))))
           'Score))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some MIDI callbacks -- is this a good place for them?

(define-public (breathe::midi-length len context)
  ;;Shorten by half, or by up to a second, but always by a power of 2
  (let* ((desired (min (ly:moment-main (seconds->moment 1 context))
                       (* (ly:moment-main len) 1/2)))
         (scale (inexact->exact (ceiling (/ (log desired) (log 1/2)))))
         (breath (ly:make-moment (expt 1/2 scale))))
    (ly:moment-sub (ly:make-moment (ly:moment-main len)) breath)))
