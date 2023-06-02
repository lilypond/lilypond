;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2014--2023 Daniel Eble <nine.fierce.ballads@gmail.com>
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

(define-public (ly:time-signature::print grob)
  "Print routine for time signatures."
  (let ((fraction (ly:grob-property grob 'fraction #f)))
    (if fraction
        (let* ((style (ly:grob-property grob 'style 'default))
               (proc (assoc-get style time-signature-style-markup-procedures))
               (fraction-markup
                (if (procedure? proc)
                    (proc fraction)
                    (make-glyph-time-signature-markup style fraction))))
          (grob-interpret-markup grob fraction-markup))
        (ly:grob-property grob 'senza-misura-stencil))))

(define-public (ly:time-signature::print-x grob)
  "Print routine for an X-shaped sign indicating no time signature."
  ;; TODO: Replace this kludge with a "timesig.X" glyph.
  (let* ((slash (ly:font-get-glyph (ly:grob-default-font grob)
                                   "noteheads.s2slash"))
         (width (interval-length (ly:stencil-extent slash X))))
    ;; overstrike the slash and its reflection
    (ly:stencil-add slash (ly:stencil-translate-axis
                           (ly:stencil-scale slash -1 1) width X))))

(define-public (add-simple-time-signature-style style proc)
  "Specify the procedure @var{proc} returning markup for a time signature
style @var{style}.  The procedure is called with one argument, the
pair @code{(@var{numerator} . @var{denominator})}."
  (set! time-signature-style-markup-procedures
        (acons style proc time-signature-style-markup-procedures)))

(define-session time-signature-style-markup-procedures `())
