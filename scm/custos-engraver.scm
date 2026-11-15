;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2026 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(define-public (Custos_engraver context)
;;; This engraver creates custos symbols.
  (let ((pitches '()))
    (make-engraver
     (acknowledgers
      ((note-head-interface engraver notehead source-engraver)
       ;; Ideally, we'd do (ly:grob-set-parent! custos Y notehead),
       ;; but since the note head lives on the other system, we can't.
       ;;
       ;; So we copy the position from the note head pitch.  We
       ;; don't look at the staff-position, since we can't be sure
       ;; whether Clef_engraver already applied a vertical shift.
       (let ((ev (event-cause notehead)))
         (when (and (ly:stream-event? ev)
                    (ly:in-event-class? ev 'note-event))
           (let ((pitch (ly:event-property ev 'pitch)))
             (when (ly:pitch? pitch)
               (set! pitches (cons pitch pitches))))))))
     ((process-acknowledged engraver)
      ;; Don't create Custos where they wouldn't be visible anyway,
      ;; i.e. only create Custos where we can have a line break.
      (when (or (not (ly:context-property context 'forbidBreak #f))
                (ly:context-property context 'forceBreak #f))
        (for-each
         (lambda (pitch)
           (let ((custos (ly:engraver-make-item engraver 'Custos '()))
                 (staff-pos (+ (ly:pitch-steps pitch)
                               (ly:context-property context 'middleCPosition 0))))
             (ly:grob-set-property! custos 'staff-position staff-pos)))
         pitches)
        (set! pitches '())))
     ((stop-translation-timestep engraver)
      (set! pitches '())))))

(ly:register-translator
 Custos_engraver 'Custos_engraver
 '((events-accepted . ())
   (grobs-created . (Custos))
   (properties-read . (forbidBreak forceBreak middleCPosition))
   (properties-written . ())
   (description . "Engrave custodes.")))
