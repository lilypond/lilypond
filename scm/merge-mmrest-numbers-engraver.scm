;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2020--2026 Jonas Hahnfeld <hahnjo@hahnjo.de>
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

(define-public (Merge_mmrest_numbers_engraver context)
  (define (text-equal? a b)
    (equal?
     (ly:grob-property a 'text)
     (ly:grob-property b 'text)))

  (let ((mmrest-numbers '()))
    (make-engraver
     ((start-translation-timestep translator)
      (set! mmrest-numbers '()))
     (acknowledgers
      ((multi-measure-rest-number-interface engraver grob source-engraver)
       (set! mmrest-numbers (cons grob mmrest-numbers))))
     ((stop-translation-timestep translator)
      (if (and (has-at-least-two? mmrest-numbers)
               (all-equal? mmrest-numbers text-equal?))
          (for-each ly:grob-suicide! (cdr (reverse mmrest-numbers))))))))

(ly:register-translator
 Merge_mmrest_numbers_engraver 'Merge_mmrest_numbers_engraver
 '((grobs-created . ())
   (events-accepted . ())
   (properties-read . ())
   (properties-written . ())
   (description . "\
Engraver to merge multi-measure rest numbers in multiple voices.

This works by gathering all multi-measure rest numbers at a time step. If they
all have the same text and there are at least two only the first one is retained
and the others are hidden.")))
