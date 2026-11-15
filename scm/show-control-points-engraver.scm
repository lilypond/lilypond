;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2021--2026 Jean Abou Samra <jean@abou-samra.fr>
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

(define (Show_control_points_engraver context)
  ;; The usual ties and slurs are spanners, but semi-ties
  ;; (laissez vibrer and repeat ties) are items.  We create
  ;; grobs of either class accordingly, with ly:engraver-make-sticky.
  (let ((beziers-found '()))
    (make-engraver
     (acknowledgers
      ;; Keep the origin engraver of each Bézier so as to
      ;; create the control points from it.  Rationale:
      ;; the engraver is in Score because otherwise its
      ;; process-acknowledged slot could be called before
      ;; the Tweak_engraver has had a chance to set
      ;; the show-control-points property.  Having it
      ;; in Score also makes \vshape work everywhere including
      ;; in custom context types where (for instance) the
      ;; Slur_engraver is consisted.  Creating the control
      ;; points from the origin engraver allows overrides
      ;; to be directed to the same context as slurs, and
      ;; could be useful for a custom engraver acknowledging
      ;; the control points.
      ((bezier-curve-interface engraver bezier source-engraver)
       (set! beziers-found
             (cons (cons bezier source-engraver)
                   beziers-found))))
     ((process-acknowledged engraver)
      (for-each
       (lambda (bezier-engraver-pair)
         (let ((bezier (car bezier-engraver-pair))
               (source-engraver (cdr bezier-engraver-pair)))
           (if (ly:grob-property bezier 'show-control-points #f)
               (begin
                                        ; Create control polygon.
                 (let ((polygon
                        (ly:engraver-make-sticky source-engraver
                                                 'ControlPolygon
                                                 bezier
                                                 bezier)))
                   (ly:grob-set-object! polygon 'bezier bezier))
                                        ; Create four control points.
                 (for-each
                  (lambda (i)
                    (let ((point
                           (ly:engraver-make-sticky source-engraver
                                                    'ControlPoint
                                                    bezier
                                                    bezier)))
                      (ly:grob-set-property! point 'index i)
                      (ly:grob-set-object! point 'bezier bezier)))
                  (iota 4))))))
       beziers-found)
      (set! beziers-found '())))))

(ly:register-translator
 Show_control_points_engraver 'Show_control_points_engraver
 '((grobs-created . (ControlPoint ControlPolygon))
   (events-accepted . ())
   (properties-read . ())
   (properties-written . ())
   (description . "Create grobs to visualize control points of Bézier
curves (ties and slurs) for ease of tweaking.")))
