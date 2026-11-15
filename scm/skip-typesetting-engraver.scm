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

(define (Skip_typesetting_engraver context)
  (let ((ellipsis #f)
        (first-time? #t)
        (was-skipping? #f))
    (make-engraver

     ((process-music engraver)
      ;; If we created an ellipsis in a prior time step, we know now that it
      ;; doesn't qualify as a STOP ellipsis since we're typesetting again.
      (set! ellipsis #f)
      (when was-skipping?
        (set! ellipsis (ly:engraver-make-grob engraver 'StaffEllipsis '()))
        ;; If we haven't done any typesetting yet, this ellipsis qualifies as a
        ;; START ellipsis.
        (when first-time?
          (ly:grob-set-property! ellipsis 'passage-direction START)))
      (set! first-time? #f))

     ((stop-translation-timestep engraver)
      (set! was-skipping? (ly:context-property context 'skipTypesetting #f)))

     ((finalize engraver)
      (when ellipsis
        (ly:grob-set-property! ellipsis 'passage-direction STOP))))))

(ly:register-translator
 Skip_typesetting_engraver 'Skip_typesetting_engraver
 '((grobs-created . (StaffEllipsis))
   (events-accepted . ())
   (properties-read . (skipTypesetting))
   (properties-written . ())
   (description . "Create a @code{StaffEllipsis} when
@code{skipTypesetting} is used.")))
