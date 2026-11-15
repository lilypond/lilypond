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

(define-public (Breathing_sign_engraver context)
  (let ((breathing-event #f))
    (make-engraver
     (listeners
      ((breathing-event engraver event)
       (set! breathing-event event)))

     ((process-music engraver)
      (if (ly:stream-event? breathing-event)
          (let ((b-type (ly:context-property context 'breathMarkType)))
            (if (symbol? b-type)
                (let ((grob (ly:engraver-make-grob
                             engraver 'BreathingSign breathing-event)))
                  (ly:breathing-sign::set-breath-properties
                   grob context b-type))))))

     ((stop-translation-timestep engraver)
      (set! breathing-event #f)))))

(ly:register-translator
 Breathing_sign_engraver 'Breathing_sign_engraver
 '((grobs-created . (BreathingSign))
   (events-accepted . (breathing-event))
   (properties-read . (breathMarkType))
   (properties-written . ())
   (description . "Notate breath marks.")))
