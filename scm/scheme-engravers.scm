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


(define-public (Measure_counter_engraver context)
  "This engraver numbers ranges of measures, which is useful in parts as an
aid for counting repeated measures.  There is no requirement that the
affected measures be repeated, however.  The user delimits the area to
receive a count with @code{\\startMeasureCount} and
@code{\\stopMeasureCount}.

Each element of a count is a spanner, and a count is thus a series of
spanners.  Each spanner is bounded by the first @code{CommandColumn} of
successive measures, and boundaries are shared by adjoining spanners."
  (let ((count-spanner '()) ; a single element of the count
        (go? #f) ; is the count in progress?
        (stop? #f) ; do we end the count?
        (last-measure-seen 0)
        (new-measure? #f)
        (elapsed 0))

    (make-engraver
     (listeners ((measure-counter-event engraver event)
                 (set! last-measure-seen (ly:context-property context 'currentBarNumber))
                 (set! new-measure? #t)
                 (cond
                  ((and (= START (ly:event-property event 'span-direction))
                        go?)
                   (begin
                     (set! stop? #t)
                     (ly:input-warning
                      (ly:event-property event 'origin)
                      "count not ended before another begun")))
                  ((= START (ly:event-property event 'span-direction))
                   (set! go? #t))
                  ((= STOP (ly:event-property event 'span-direction))
                   (begin
                     (set! stop? #t)
                     (set! go? #f))))))

     ((process-music trans)
      (let ((col (ly:context-property context 'currentCommandColumn))
            (now (ly:context-property context 'measurePosition))
            (current-bar (ly:context-property context 'currentBarNumber)))
        ;; If the counter has been started, make sure we're in a new bar
        ;; before finishing a count-spanner and starting a new one.
        ;; Since we consider all CommandColumns encountered, we need this
        ;; check so that a count-spanner is not created for each pair.
        (if (and (ly:grob? count-spanner)
                 (> current-bar last-measure-seen))
            (set! new-measure? #t))
        (if new-measure?
            (begin
              ;; Check if we have the first column of the measure.
              ;; The possibility of initial grace notes is considered.
              (if (moment<=? now ZERO-MOMENT)
                  (begin
                    ;; If we have the first column, finish the previous
                    ;; counter-spanner (if there is one).
                    (if (ly:grob? count-spanner)
                        (begin
                          (ly:spanner-set-bound! count-spanner RIGHT col)
                          (ly:pointer-group-interface::add-grob count-spanner 'columns col)
                          (ly:engraver-announce-end-grob trans count-spanner col)
                          (set! count-spanner '())))
                    ;; if count is over, reset variables
                    (if stop?
                        (begin
                          (set! elapsed 0)
                          (set! stop? #f)))
                    ;; if count is in progress, begin a counter object
                    (if go?
                        (let* ((c (ly:engraver-make-grob trans 'MeasureCounter col))
                               (counter (ly:grob-property c 'count-from)))
                          (ly:spanner-set-bound! c LEFT col)
                          (ly:pointer-group-interface::add-grob c 'columns col)
                          (set! (ly:grob-property c 'count-from) (+ counter elapsed))
                          (set! count-spanner c)
                          (set! elapsed (1+ elapsed))))
                    (set! new-measure? #f)))))
        (set! last-measure-seen current-bar)))

     ((finalize trans)
      (if go?
          (begin
            (set! go? #f)
            (ly:grob-suicide! count-spanner)
            (set! count-spanner '())
            (ly:warning "measure count left unfinished")))))))
