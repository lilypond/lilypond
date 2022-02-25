;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define (make-percent-set music)
  "Compute the elements of percent-repeated music, consisting of the
body followed by a placeholder element for each repetition.  The
iterator will generate the actual percent events."
  (let ((n (ly:music-property music 'repeat-count))
        (body (ly:music-property music 'element)))
    (if (> n 1)
        ;; Percent repeats normally have a nonzero main length.  If
        ;; they begin with grace notes, we disregard them and place
        ;; the percent event at the start of the main notes.
        ;;
        ;; It's possible to write ly code for a percent repeat with
        ;; nothing but grace notes in the body; in that case, we
        ;; create a grace-time skip.  That doesn't guarantee that
        ;; everything else will handle the music sensibly, but the
        ;; timekeeping will be correct.
        (let* ((length (ly:music-length body))
               (start (if (= 0 (ly:moment-main length))
                          (ly:music-start body)
                          ZERO-MOMENT))
               (placeholder (skip-of-moment-span start length)))
          (cons body (make-list (- n 1) placeholder)))
        (list body))))

(define (make-tremolo-set tremolo)
  "Given a tremolo repeat, return a list of music to engrave for it.
This will be a stretched copy of its body, plus a TremoloEvent or
TremoloSpanEvent."
  (define (first-note-duration music)
    "Finds the duration of the first NoteEvent by searching
depth-first through MUSIC."
    ;; NoteEvent or a non-expanded chord-repetition
    ;; We just take anything that actually sports an announced duration.
    (if (ly:duration? (ly:music-property music 'duration))
        (ly:music-property music 'duration)
        (let loop ((elts (if (ly:music? (ly:music-property music 'element))
                             (list (ly:music-property music 'element))
                             (ly:music-property music 'elements))))
          (and (pair? elts)
               (let ((dur (first-note-duration (car elts))))
                 (if (ly:duration? dur)
                     dur
                     (loop (cdr elts))))))))
  (let* ((times (ly:music-property tremolo 'repeat-count))
         (body (ly:music-property tremolo 'element))
         (children (if (music-is-of-type? body 'sequential-music)
                       ;; \repeat tremolo n { ... }
                       (count duration-of-note ; do not count empty <>
                              (extract-named-music body
                                                   '(EventChord NoteEvent)))
                       ;; \repeat tremolo n c4
                       1))
         (tremolo-type (if (positive? children)
                           (let* ((note-duration (first-note-duration body))
                                  (duration-log
                                   (if (ly:duration? note-duration)
                                       (ly:duration-log note-duration)
                                       1)))
                             (ash 1 duration-log))
                           '()))
         (stretched (ly:music-deep-copy body)))
    (if (positive? children)
        ;; # of dots is equal to the 1 in bitwise representation (minus 1)!
        (let* ((dots (1- (logcount (* times children))))
               ;; The remaining missing multiplier to scale the notes by
               ;; times * children
               (mult (/ (* times children (ash 1 dots)) (1- (ash 2 dots))))
               (shift (- (ly:intlog2 (floor mult)))))
          (if (not (and (integer? mult) (= (logcount mult) 1)))
              (ly:music-warning
               body
               (format #f (G_ "invalid tremolo repeat count: ~a") times)))
          ;; Make each note take the full duration
          (ly:music-compress stretched (ly:make-moment 1 children))
          ;; Adjust the displayed note durations
          (shift-duration-log stretched shift dots)))
    ;; Return the stretched body plus a tremolo event
    (if (= children 1)
        (list (make-music 'TremoloEvent
                          'repeat-count times
                          'tremolo-type tremolo-type
                          'origin (ly:music-property tremolo 'origin))
              stretched)
        (list (make-music 'TremoloSpanEvent
                          'span-direction START
                          'repeat-count times
                          'tremolo-type tremolo-type
                          'origin (ly:music-property tremolo 'origin))
              stretched
              (make-music 'TremoloSpanEvent
                          'span-direction STOP
                          'origin (ly:music-property tremolo 'origin))))))

(define (make-unfolded-set music)
  (let* ((n (ly:music-property music 'repeat-count))
         (alts (ly:music-property music 'elements))
         (body (ly:music-property music 'element))
         (volte (list-tabulate
                 n
                 (lambda (i)
                   (make-sequential-music
                    (cons (ly:music-deep-copy body)
                          (ly:music-deep-copy alts)))))))

    (define (pass-over-repeated-music music)
      (not (music-is-of-type? music 'repeated-music)))

    (define (keep-for-volta music n)
      ;; TODO: It might be helpful to warn about a volta specification
      ;; that is outside [1, repeat_count], but there are things to
      ;; consider.
      ;;
      ;; A check here would only notice as repeats are unfolded.  It
      ;; would not notice when repeats are left in volta notation or
      ;; when \volta is used outside of a repeat.  A consistent,
      ;; general check might require making music iterators aware of
      ;; scoped music properties. (As I am iterating, is there a
      ;; repeat count in enclosing music; if so, what?)
      ;;
      ;; No warning should be issued for an empty set.  That is useful
      ;; for removing the music unconditionally when unfolded.
      (or (not (music-is-of-type? music 'volta-specification))
          (pair? (member n (ly:music-property music 'volta-numbers '())))))

    (define (unwrap-for-volta music)
      (cond ((music-is-of-type? music 'sequential-alternative-music)
             (make-sequential-music (ly:music-property music 'elements)))
            ((music-is-of-type? music 'volta-specification)
             (ly:music-property music 'element))
            ((music-is-of-type? music 'unfolded-specification)
             (ly:music-property music 'element))
            (else
             music)))

    (for-each (lambda (volta-num volta-music)
                ;; discard all music that is not for this volta
                (set! volta-music
                      (music-selective-filter
                       pass-over-repeated-music
                       (lambda (m) (keep-for-volta m volta-num))
                       volta-music))
                ;; discard the remaining VoltaSpeccedMusic and
                ;; SequentialAlternativeMusic wrappers
                (set! volta-music
                      (music-selective-map
                       pass-over-repeated-music
                       unwrap-for-volta
                       volta-music)))

              (iota n 1 1) volte)

    volte))

(define (make-volta-set music)
  (let* ((alts (ly:music-property music 'elements))
         (body (ly:music-property music 'element)))
    (cons
     body
     (list (make-music
            'SequentialAlternativeMusic
            'elements alts)))))

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
