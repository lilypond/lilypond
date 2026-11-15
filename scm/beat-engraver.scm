;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2021--2026 David Kastrup <dak@gnu.org>
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

;; This relies on Beat_performer from beat-performer.scm, which must be
;; loaded first.
(ly:register-translator
 (lambda (c) `((is-midi . #f) (is-layout . #t) ,@(Beat_performer c))) 'Beat_engraver
 '((events-accepted . (note-event articulation-event))
   (properties-read . (timing
                       measurePosition
                       beatBase
                       beatStructure
                       timeSignature
                       barExtraVelocity
                       beatExtraVelocity))
   (properties-written . ())
   (description . "\
This engraver is just a functionally identical copy of
@ref{Beat_performer}, used for visualizing its effects.  You can also
use it for showcasing the effects of the current @code{beatStructure}.")))
