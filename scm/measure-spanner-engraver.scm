;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2017--2026 David Nalesnik <david.nalesnik@gmail.com>
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

(define-public (Measure_spanner_engraver context)
  (let ((span '())
        (finished '())
        (event-start '())
        (event-stop '()))
    (make-engraver
     (listeners ((measure-spanner-event engraver event)
                 (if (= START (ly:event-property event 'span-direction))
                     (set! event-start event)
                     (set! event-stop event))))
     ((process-music trans)
      (if (ly:stream-event? event-stop)
          (if (null? span)
              (ly:warning (G_ "cannot find start of measure spanner"))
              (begin
                (set! finished span)
                (ly:engraver-announce-end-grob trans finished event-start)
                (set! span '())
                (set! event-stop '()))))
      (if (ly:stream-event? event-start)
          (begin
            (set! span (ly:engraver-make-grob trans 'MeasureSpanner event-start))
            (set! event-start '()))))
     ((stop-translation-timestep trans)
      (if (and (ly:spanner? span)
               (not (ly:spanner-bound span LEFT #f))
               (moment<=? (ly:context-property context 'measurePosition) ZERO-MOMENT))
          (ly:spanner-set-bound! span LEFT
                                 (ly:context-property context 'currentCommandColumn)))
      (if (and (ly:spanner? finished)
               (moment<=? (ly:context-property context 'measurePosition) ZERO-MOMENT))
          (begin
            (if (not (ly:spanner-bound finished RIGHT #f))
                (ly:spanner-set-bound! finished RIGHT
                                       (ly:context-property context 'currentCommandColumn)))
            (set! finished '())
            (set! event-start '())
            (set! event-stop '()))))
     ((finalize trans)
      (if (and (ly:spanner? finished)
               (not (ly:spanner-bound finished RIGHT #f)))
          (set! (ly:spanner-bound finished RIGHT)
                (ly:context-property context 'currentCommandColumn)))
      (if (ly:spanner? span)
          (begin
            (ly:warning (G_ "unterminated measure spanner"))
            (ly:grob-suicide! span)))))))

(ly:register-translator
 Measure_spanner_engraver 'Measure_spanner_engraver
 '((grobs-created . (MeasureSpanner))
   (events-accepted . (measure-spanner-event))
   (properties-read . (measurePosition
                       currentCommandColumn))
   (properties-written . ())
   (description . "\
This engraver creates spanners bounded by the columns that start and
end measures in response to @code{\\startMeasureSpanner} and
@code{\\stopMeasureSpanner}.")))
