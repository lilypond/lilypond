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

;; TODO: yet another engraver for alignment... Ultimately, it would be nice to
;; merge Dynamic_align_engraver, Piano_pedal_align_engraver and
;; Centered_bar_number_align_engraver.
(define-public (Centered_bar_number_align_engraver context)
  (let ((support-line #f))
    (make-engraver
     (acknowledgers
      ((centered-bar-number-interface engraver grob source-engraver)
       ;; Create the support spanner on the fly when we meet a first
       ;; centered bar number, to avoid an extra grob in the most
       ;; common case.
       (if (not support-line)
           (begin
             (set! support-line
                   (ly:engraver-make-grob engraver
                                          'CenteredBarNumberLineSpanner
                                          '()))
             (ly:spanner-set-bound!
              support-line
              LEFT
              (ly:context-property context 'currentCommandColumn))))
       (ly:axis-group-interface::add-element support-line grob)))
     ((finalize engraver)
      (if support-line
          (ly:spanner-set-bound!
           support-line
           RIGHT
           (ly:context-property context 'currentCommandColumn)))))))

(ly:register-translator
 Centered_bar_number_align_engraver 'Centered_bar_number_align_engraver
 '((grobs-created . (CenteredBarNumberLineSpanner))
   (events-accepted . ())
   (properties-read . (currentCommandColumn))
   (properties-written . ())
   (description . "Group measure-centered bar numbers in a
@code{CenteredBarNumberLineSpanner} so they end up on the same
vertical position.")))
