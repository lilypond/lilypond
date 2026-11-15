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

(define (Alteration_glyph_engraver context)
  (make-engraver
   (acknowledgers
    ((accidental-switch-interface engraver grob source-engraver)
     (let ((alteration-glyphs
            (ly:context-property context 'alterationGlyphs)))
       (if alteration-glyphs
           (ly:grob-set-property! grob
                                  'alteration-glyph-name-alist
                                  alteration-glyphs)))))))

(ly:register-translator
 Alteration_glyph_engraver 'Alteration_glyph_engraver
 '((grobs-created . ())
   (events-accepted . ())
   (properties-read . (alterationGlyphs))
   (properties-written . ())
   (description . "Set the @code{glyph-name-alist} of all grobs having the
@code{accidental-switch-interface} to the value of the context's
@code{alterationGlyphs} property, when defined.")))
