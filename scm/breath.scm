;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022 Daniel Eble <nine.fierce.ballads@gmail.com>
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

(define-public default-breath-alist
  `(
    (altcomma
     . (
        (text . ,(make-musicglyph-markup "scripts.raltcomma"))))
    (caesura
     . (
        (text . ,(make-musicglyph-markup "scripts.caesura.straight"))))
    (chantdoublebar
     . (
        (extra-spacing-width . (-1.0 . 0.0))
        (stencil . ,ly:breathing-sign::finalis)
        (Y-offset . 0.0)))
    (chantfullbar
     . (
        (extra-spacing-width . (-1.0 . 0.0))
        (stencil . ,ly:breathing-sign::divisio-maxima)
        (Y-offset . 0.0)))
    (chanthalfbar
     . (
        (extra-spacing-height . ,item::extra-spacing-height-including-staff)
        (extra-spacing-width . (-1.0 . 0.0))
        (stencil . ,ly:breathing-sign::divisio-maior)
        (Y-offset . 0.0)))
    (chantquarterbar
     . (
        (extra-spacing-height . ,item::extra-spacing-height-including-staff)
        (extra-spacing-width . (-1.0 . 0.0))
        (stencil . ,ly:breathing-sign::divisio-minima)))
    (comma
     . (
        (text . ,(make-musicglyph-markup "scripts.rcomma"))))
    (curvedcaesura
     . (
        (text . ,(make-musicglyph-markup "scripts.caesura.curved"))))
    (outsidecomma
     . (
        (outside-staff-priority . 40) ; same as MultiMeasureRestScript
        (text . ,(make-musicglyph-markup "scripts.rcomma"))
        ))
    (spacer
     . (
        (text . ,(make-null-markup))))
    (tickmark
     . (
        (outside-staff-priority . 40) ; same as MultiMeasureRestScript
        (text . ,(make-musicglyph-markup "scripts.tickmark"))
        ))
    (upbow
     . (
        (outside-staff-priority . 40) ; same as MultiMeasureRestScript
        (text . ,(make-musicglyph-markup "scripts.upbow"))
        ))
    (varcomma
     . (
        (text . ,(make-musicglyph-markup "scripts.rvarcomma"))))
    ))
