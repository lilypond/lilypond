;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2012 David Nalesnik <david.nalesnik@gmail.com>
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

(define-public (ly:make-listener callback)
  "This is a compatibility wrapper for creating a \"listener\" for use
with @code{ly:add-listener} from a @var{callback} taking a single
argument.  Since listeners are equivalent to callbacks, this is no
longer needed."
  callback)

(define-public (Measure_counter_engraver context)
  "This engraver numbers ranges of measures, which is useful in parts as an
aid for counting repeated measures.  There is no requirement that the
affected measures be repeated, however.  The user delimits the area to
receive a count with @code{\\startMeasureCount} and
@code{\\stopMeasureCount}."
  (let ((count-spanner '()) ; a single element of the count
        (go? #f) ; is the count in progress?
        (stop? #f) ; do we end the count?
        (last-measure-seen 0)
        (elapsed 0))

    (make-engraver
     (listeners
      ((measure-counter-event engraver event)
       (cond
        ((and (= START (ly:event-property event 'span-direction))
              go?)
         (set! stop? #t)
         (ly:input-warning
          (ly:event-property event 'origin)
          "count not ended before another begun"))
        ((= START (ly:event-property event 'span-direction))
         (set! go? #t)
         ;; initialize one less so first measure receives a count spanner
         (set! last-measure-seen
               (1- (ly:context-property context 'currentBarNumber))))
        ((= STOP (ly:event-property event 'span-direction))
         (set! stop? #t)
         (set! go? #f)))))

     ((process-music trans)
      (let ((col (ly:context-property context 'currentCommandColumn))
            (now (ly:context-property context 'measurePosition))
            (current-bar (ly:context-property context 'currentBarNumber)))
        ;; Each measure of a count receives a new spanner, which is bounded
        ;; by the first "command column" of that measure and the following one.
        ;; The possibility of initial grace notes (negative measure position)
        ;; is considered.
        (if (and (> current-bar last-measure-seen)
                 (moment<=? now ZERO-MOMENT))
            (begin
              ;; Finish the previous count-spanner if there is one.
              (if (ly:grob? count-spanner)
                  (begin
                    (ly:spanner-set-bound! count-spanner RIGHT col)
                    (ly:pointer-group-interface::add-grob count-spanner 'columns col)
                    (ly:engraver-announce-end-grob trans count-spanner col)
                    (set! count-spanner '())))
              ;; If count is over, reset variables.
              (if stop?
                  (begin
                    (set! elapsed 0)
                    (set! stop? #f)))
              ;; If count is in progress, begin a count-spanner.
              (if go?
                  (let* ((c (ly:engraver-make-grob trans 'MeasureCounter col))
                         (counter (ly:grob-property c 'count-from)))
                    (ly:spanner-set-bound! c LEFT col)
                    (ly:pointer-group-interface::add-grob c 'columns col)
                    (set! (ly:grob-property c 'count-from) (+ counter elapsed))
                    (set! count-spanner c)
                    (set! elapsed (1+ elapsed))))))
        (set! last-measure-seen current-bar)))

     ((finalize trans)
      (if go?
          (begin
            (set! go? #f)
            (ly:grob-suicide! count-spanner)
            (set! count-spanner '())
            (ly:warning "measure count left unfinished")))))))

(ly:register-translator
 Measure_counter_engraver 'Measure_counter_engraver
 '((grobs-created . (MeasureCounter))
   (events-accepted . (measure-counter-event))
   (properties-read . (currentCommandColumn
                       measurePosition
                       currentBarNumber))
   (properties-written . ())
   (description . "\
This engraver numbers ranges of measures, which is useful in parts as an
aid for counting repeated measures.  There is no requirement that the
affected measures be repeated, however.  The user delimits the area to
receive a count with @code{\\startMeasureCount} and
@code{\\stopMeasureCount}.")))

(ly:register-translator
 Span_stem_engraver 'Span_stem_engraver
 '((grobs-created . (Stem))
   (events-accepted . ())
   (properties-read . ())
   (properties-written . ())
   (description . "Connect cross-staff stems to the stems above in the system")))
