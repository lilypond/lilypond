;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022--2026 Jean Abou Samra <jean@abou-samra.fr>
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

(define (Trill_spanner_engraver context)
  (let ((start-event #f)
        (stop-event #f)
        (trill #f)
        (ended-trill #f))
    (make-engraver
     (listeners
      ((trill-span-event engraver event)
       (if (eqv? START (ly:event-property event 'span-direction))
           (set! start-event event)
           (set! stop-event event))))
     ((process-music engraver)
      (let ((ender (or stop-event start-event)))
        (when (and ender trill)
          (set! ended-trill trill)
          (ly:engraver-announce-end-grob engraver ended-trill ender)
          (set! trill #f)))
      (when start-event
        (set! trill (ly:engraver-make-grob engraver 'TrillSpanner start-event))
        (ly:side-position-interface::set-axis! trill Y)
        (let ((direction (ly:event-property start-event 'direction)))
          (when (ly:dir? direction)
            (ly:grob-set-property! trill 'direction direction)))
        (when ended-trill
          (ly:grob-set-object! ended-trill 'right-neighbor trill))))
     (acknowledgers
      ((note-column-interface engraver grob source-engraver)
       ;; If we find a note column, use it for the left bound of the
       ;; newly created trill and the right bound of the trill that
       ;; ended here.
       (when trill
         (ly:pointer-group-interface::add-grob trill 'note-columns grob)
         (ly:pointer-group-interface::add-grob trill 'side-support-elements grob)
         (when start-event
           (ly:spanner-set-bound! trill LEFT grob)))
       (when ended-trill
         (ly:pointer-group-interface::add-grob ended-trill 'note-columns grob)
         (ly:pointer-group-interface::add-grob ended-trill 'side-support-elements grob)
         (when (not (ly:spanner-bound ended-trill RIGHT #f)) ; respect to-barline
           (ly:spanner-set-bound! ended-trill RIGHT grob)))))
     ((stop-translation-timestep engraver)
      ;; Absent any note column, fall back on the musical paper column.
      (when (and trill
                 ;; A bound already set is a note column, added above.
                 (not (ly:spanner-bound trill LEFT #f)))
        (let ((col (ly:context-property context 'currentMusicalColumn)))
          (ly:spanner-set-bound! trill LEFT col)))
      (when (and ended-trill
                 ;; A bound already set is either a note column, added
                 ;; above, or a BarLine due to to-barline.
                 (not (ly:spanner-bound ended-trill RIGHT #f)))
        (let ((col (ly:context-property context 'currentMusicalColumn)))
          (ly:spanner-set-bound! ended-trill RIGHT col))
        (set! ended-trill #f))
      (set! start-event #f)
      (set! stop-event #f))
     ((finalize engraver)
      (when trill
        ;; Fix the bound of a trill extending to the end of the piece:
        ;; the musical column is past the end of the score.  Use the
        ;; non-musical column instead.
        (let ((col (ly:context-property context 'currentCommandColumn)))
          (ly:spanner-set-bound! trill RIGHT col)))))))

(ly:register-translator
 Trill_spanner_engraver 'Trill_spanner_engraver
 '((events-accepted . (trill-span-event))
   (grobs-created . (TrillSpanner))
   (properties-read . (currentCommandColumn currentMusicalColumn))
   (properties-written . ())
   (description . "Create trill spanners.")))
