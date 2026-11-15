;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define-public Measure_grouping_engraver
  (lambda (ctx)
    (let ((grouping-spanner #f)
          (stop-grouping-moment #f))
      (make-engraver
       ((process-music engraver)
        (let ((now (ly:context-current-moment ctx)))
          (when (zero? (ly:moment-grace now))
            (when (and grouping-spanner
                       (not (ly:moment<? now stop-grouping-moment)))
              (ly:spanner-set-bound!
               grouping-spanner
               RIGHT
               (ly:context-property ctx 'currentMusicalColumn))
              (set! grouping-spanner #f))

            (let*
                ((beat-base (ly:context-property ctx 'beatBase))
                 (measure-position (ly:context-property ctx 'measurePosition)))
              (let loop ((where ZERO-MOMENT)
                         (grouping (ly:context-property ctx 'beatStructure)))
                (when (and (equal? where measure-position)
                           (pair? grouping))
                  (ly:context-schedule-moment
                   ;; make sure the next grouping step will be encountered
                   ;; by engravers (i.e. by this one)
                   ctx
                   (+ now (ly:make-moment (* beat-base (car grouping)))))
                  (if grouping-spanner
                      (ly:programming-error
                       "last grouping-spanner not finished yet")
                      (when (> (car grouping) 1)
                        (set! grouping-spanner (ly:engraver-make-grob
                                                engraver 'MeasureGrouping '()))
                        (ly:spanner-set-bound!
                         grouping-spanner LEFT
                         (ly:context-property ctx 'currentMusicalColumn))
                        (set! stop-grouping-moment
                              (+ now (ly:make-moment
                                      (* beat-base (1- (car grouping))))))
                        (ly:context-schedule-moment ctx stop-grouping-moment)
                        (ly:grob-set-property!
                         grouping-spanner 'style
                         (if (= (car grouping) 3) 'triangle 'bracket))
                        (set! grouping '()))))
                (when (pair? grouping)
                  (loop
                   (+ where (ly:make-moment (* beat-base (car grouping))))
                   (cdr grouping))))))))
       (acknowledgers
        ((note-column-interface engraver grob source-engraver)
         (when grouping-spanner
           (ly:pointer-group-interface::add-grob grouping-spanner
                                                 'side-support-elements
                                                 grob))))
       ((finalize engraver)
        (when grouping-spanner
          (ly:spanner-set-bound!
           grouping-spanner
           RIGHT
           (ly:context-property ctx 'currentMusicalColumn))
          (ly:grob-suicide! grouping-spanner)))))))

(ly:register-translator
 Measure_grouping_engraver 'Measure_grouping_engraver
 '((events-accepted . ())
   (grobs-created . (MeasureGrouping))
   (properties-read . (beatBase
                       beatStructure
                       currentMusicalColumn
                       measurePosition))
   (properties-written . ())
   (description . "Create @code{MeasureGrouping} to indicate beat subdivision.")))
