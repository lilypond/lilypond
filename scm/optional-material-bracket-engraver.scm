;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022--2026 Dan Eble <nine.fierce.ballads@gmail.com>
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

(define-public (Optional_material_bracket_engraver context)
  (let ((start-event #f)
        (end-event #f))
    (make-engraver
     (listeners
      ((optional-material-event engraver event)
       (if (= START (ly:event-property event 'span-direction))
           (set! start-event event)
           (set! end-event event))))

     ((process-music engraver)
      (when (ly:stream-event? end-event)
        (let ((grob (ly:engraver-make-grob engraver 'OptionalMaterialBracket
                                           end-event)))
          (ly:grob-set-property! grob 'passage-direction STOP)))
      (when (ly:stream-event? start-event)
        (let ((grob (ly:engraver-make-grob engraver 'OptionalMaterialBracket
                                           start-event)))
          (ly:grob-set-property! grob 'passage-direction START))))

     ((stop-translation-timestep engraver)
      (set! start-event #f)
      (set! end-event #f)))))

(ly:register-translator
 Optional_material_bracket_engraver 'Optional_material_bracket_engraver
 '((grobs-created . (OptionalMaterialBracket))
   (events-accepted . (optional-material-event))
   (properties-read . ())
   (properties-written . ())
   (description . "Notate in-staff brackets for optional material.")))
