;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2021--2022 David Kastrup <dak@gnu.org>
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

(define (Beat_performer context)
  (define (closest-beat ctx mp)
    ;; return the first on-beat measure position not before mp
    (let* ((mp (ly:moment-main mp))
           (tf (ly:context-property ctx 'timeSignatureFraction '(4 . 4)))
           (bmm (ly:context-property ctx 'baseMoment))
           (bm (if (ly:moment? bmm)
                   (ly:moment-main bmm)
                   (/ (cdr tf))))
           (bs (ly:context-property ctx 'beatStructure)))
      (let loop ((pos 0) (bs bs))
        (cond ((>= pos mp) (ly:make-moment pos))
              ((pair? bs)
               (loop (+ pos (* bm (car bs))) (cdr bs)))
              (else
               ;; pos is an integral multiple of baseMoment and
               ;; still smaller than measurePosition, so after
               ;; having exhausted any possible beatStructure
               ;; without passing measurePosition, just rounding
               ;; measurePosition up to the next multiple of
               ;; baseMoment will be the correct answer without
               ;; requiring us to loop or to even consider the
               ;; iteration variable pos
               (ly:make-moment (* bm (ceiling (/ mp bm)))))))))
  (define fired #f)
  (define timeout #f)
  (define (emit ctx strong?)
    (set! fired
          (ly:make-stream-event
           (ly:make-event-class 'articulation-event)
           `(
             ;; We differentiate the "visuals" of the generated
             ;; events for debugging purposes: the performer is not
             ;; intended to be used while typesetting, but a
             ;; wrapper may add an "is-layout" property
             (articulation-type . ,(if strong? 'marcato 'accent))
             (midi-extra-velocity
              . ,(+ (or (ly:context-property ctx 'beatExtraVelocity 15) 0)
                    (or (and strong? (ly:context-property ctx 'barExtraVelocity 10))
                        0))))))
    (ly:broadcast (ly:context-event-source ctx) fired))

  (make-performer
   ((stop-translation-timestep c) (set! fired #f))
   ((process-music performer)
    ;; No syncope tracking across cadenze
    (and timeout (not (ly:context-property context 'timing))
         (set! timeout #f)))
   (listeners
    ;; we have a listener for explicit articulation events in order
    ;; to let syncopated accents silence the next "regular" stress
    ((articulation-event performer event)
     (cond ((eq? fired event))  ; ignore our own events
           ((memq (ly:event-property event 'articulation-type)
                  '(accent marcato))
            (let ((mp (ly:context-property context
                                           'measurePosition ZERO-MOMENT))
                  (now (ly:context-current-moment context)))
              (set! timeout (ly:moment-add (closest-beat context mp)
                                           (ly:moment-sub now mp)))))))
    ;; Listener for note events.
    ((note-event performer event)
     (and (not fired)
          (ly:context-property context 'timing)
          (not (and timeout (moment<=? (ly:context-current-moment context)
                                       timeout)))
          (let ((mp (ly:context-property context 'measurePosition ZERO-MOMENT)))
            (if (equal? mp (closest-beat context mp))
                (emit context (equal? mp ZERO-MOMENT)))))))))

(ly:register-translator
 Beat_performer 'Beat_performer
 '((events-accepted . (note-event articulation-event))
   (properties-read . (timing
                       measurePosition
                       baseMoment
                       beatStructure
                       timeSignatureFraction
                       barExtraVelocity
                       beatExtraVelocity))
   (properties-written . ())
   (description . "\
This performer is intended for instantiation in @samp{Voice}-like
contexts.  The context variable @code{beatExtraVelocity}
is used for adding extra MIDI velocity at each beat (default 15) in
accordance with @code{beatStructure} and an additional
@code{barExtraVelocity} (default 10) at the start of each bar.

This is done by adding corresponding @code{\\accent} and
@code{\\marcato} events when such note events are encountered.

Off-beat manual use of @code{\\accent} or @code{\\marcato} causes
autogeneration of the next on-beat accent to be skipped.")))

(ly:register-translator
 (lambda (c) `((is-midi . #f) (is-layout . #t) ,@(Beat_performer c))) 'Beat_engraver
 '((events-accepted . (note-event articulation-event))
   (properties-read . (timing
                       measurePosition
                       baseMoment
                       beatStructure
                       timeSignatureFraction
                       barExtraVelocity
                       beatExtraVelocity))
   (properties-written . ())
   (description . "\
This engraver is just a functionally identical copy of
@ref{Beat_performer}, used for visualising its effects.  You can also
use it for showcasing the effects of the current @code{beatStructure}.")))
