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

(define (Signum_repetitionis_engraver context)
  (let ((end-event '()))
    (make-engraver
     (listeners
      ((volta-repeat-end-event engraver event #:once)
       (set! end-event event)))

     ((process-music engraver)
      (if (ly:stream-event? end-event)
          (if (= 0 (ly:event-property end-event 'alternative-number 0))
              ;; Simple repeat: Indicate the repeat count with 1 to 4
              ;; strokes.  (Thanks to Dr. Ross W. Duffin for
              ;; explaining this in his Notation Manual at
              ;; https://casfaculty.case.edu/ross-duffin/.)
              (let ((n (ly:event-property end-event 'repeat-count 0)))
                (if (positive? n)
                    (let ((grob (ly:engraver-make-grob
                                 engraver 'SignumRepetitionis end-event))
                          (glyph (case n
                                   ((1) ":,:")
                                   ((2) ":,,:")
                                   ((3) ":,,,:")
                                   ((4) ":,,,,:")
                                   (else #f))))
                      (if (string? glyph)
                          (ly:grob-set-property! grob 'glyph glyph)))))
              ;; Alternative ending: This is not a use case for this
              ;; ancient notation.  Just create a grob with the
              ;; default glyph if there is a return.
              (let ((n (ly:event-property end-event 'return-count 0)))
                (if (positive? n)
                    (ly:engraver-make-grob
                     engraver 'SignumRepetitionis end-event))))))

     ((stop-translation-timestep engraver)
      (set! end-event '())))))

(ly:register-translator
 Signum_repetitionis_engraver 'Signum_repetitionis_engraver
 '((grobs-created . (SignumRepetitionis))
   (events-accepted . (volta-repeat-end-event))
   (properties-read . ())
   (properties-written . ())
   (description . "Create a @code{SignumRepetitionis} at the end of
a @code{\\repeat volta} section.")))
