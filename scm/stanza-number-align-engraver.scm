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

(define (Stanza_number_align_engraver context)
  (let
   ((lyric-syllables '())
    (stanza-numbers '()))
   (make-engraver
    (acknowledgers
     ((lyric-syllable-interface engraver lyric-syllable source-engraver)
      (set! lyric-syllables (cons lyric-syllable lyric-syllables)))
     ((stanza-number-interface engraver stanza-number source-engraver)
      (set! stanza-numbers (cons stanza-number stanza-numbers))))
    ((stop-translation-timestep engraver)
     (for-each
      (lambda (stanza-number)
        (for-each
         (lambda (lyric-syllable)
           (ly:pointer-group-interface::add-grob stanza-number
                                                 'side-support-elements
                                                 lyric-syllable))
         lyric-syllables))
      stanza-numbers)
     (set! lyric-syllables '())
     (set! stanza-numbers '())))))

(ly:register-translator
 Stanza_number_align_engraver 'Stanza_number_align_engraver
 '((grobs-created . ())
   (events-accepted . ())
   (properties-read . ())
   (properties-written . ())
   (description . "This engraver ensures that stanza numbers are neatly
aligned across all lyrics contexts.")))
