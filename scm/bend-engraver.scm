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

(define (Bend_engraver context)
  (let
      ((stop-moment #f)
       (fall-event #f)
       (fall #f)
       (start-in-grace #f)
       (last-fall #f)
       (note-head #f))
    (define (stop-fall)
      (set! last-fall fall)
      (set! fall #f)
      (set! start-in-grace #f)
      (set! note-head #f)
      (set! fall-event #f))
    (make-engraver
     ((start-translation-timestep engraver)
      (set! last-fall #f)
      (when (and fall
                 (if start-in-grace
                     ;; For falls starting in grace time, compare actual
                     ;; moments to make sure the band spans the full (grace)
                     ;; length of the main note.
                     ;; For falls starting in non-grace time, compare only
                     ;; moment-main's to make sure the fall gets terminated
                     ;; before any grace notes coming before the next note.
                     (not (ly:moment<? (ly:context-current-moment context)
                                       stop-moment))
                     (>= (ly:moment-main (ly:context-current-moment context))
                         (ly:moment-main stop-moment))))
        (stop-fall)))
     (listeners
      ((bend-after-event engraver event #:once)
       (set! fall-event event)))
     ((process-music engraver)
      (when (and fall-event (not fall))
        (set! fall (ly:engraver-make-grob engraver 'BendAfter fall-event))
        (ly:grob-set-property! fall 'delta-position
                               (ly:event-property fall-event 'delta-step))))
     (acknowledgers
      ((note-head-interface engraver grob source-engraver)
       (when fall-event
         (when (and note-head fall)
           (stop-fall))
         (set! note-head grob)
         (let
             ((now (ly:context-current-moment context)))
           (set! start-in-grace (negative? (ly:moment-grace now)))
           (set! stop-moment
                 (ly:moment-add
                  now
                  (ly:event-length (event-cause grob) now)))))))
     ((stop-translation-timestep engraver)
      (when last-fall
        (ly:spanner-set-bound!
         last-fall
         RIGHT
         (if (ly:grob? (ly:context-property context 'currentBarLine))
             ;; don't cross a bar line!
             (ly:context-property context 'currentCommandColumn)
             (ly:context-property context 'currentMusicalColumn))))
      (when (and fall
                 (not (ly:grob? (ly:spanner-bound fall LEFT))))
        (ly:spanner-set-bound! fall LEFT note-head)
        (ly:grob-set-parent! fall Y note-head)))
     ((finalize engraver)
      (when last-fall
        (ly:spanner-set-bound!
         last-fall
         RIGHT
         (ly:context-property context 'currentCommandColumn)))))))

(ly:register-translator
 Bend_engraver 'Bend_engraver
 '((events-accepted . (bend-after-event))
   (grobs-created . (BendAfter))
   (properties-read . (currentBarLine currentCommandColumn currentMusicalColumn))
   (properties-written . ())
   (description . "Create fall spanners.")))
