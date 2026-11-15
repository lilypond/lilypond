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

(define (Grid_chord_name_engraver context)
  (let (;; Maintained as the left bound for newly created GridChordName grobs.
        ;; This is the currentCommandColumn at the start of the piece, and the
        ;; latest BarLine afterwards.
        (left-bound #f)
        ;; Was a bar line found in this time step?  Reset after each
        ;; time step, unlike left-bound.
        (bar-found #f)
        ;; GridChordName being created.
        (chord-name #f)
        ;; All grid chord names from the current measure, to be ended at the
        ;; next bar line.
        (accumulated-chord-names '()))
    (make-engraver
     ((process-music engraver)
      (when (not left-bound) ; first time step
        (set! left-bound (ly:context-property context 'currentCommandColumn)))
      (let ((text (ly:context-property context 'currentChordText #f)))
        (when text
          (let ((cause (ly:context-property context 'currentChordCause)))
            (set! chord-name (ly:engraver-make-grob engraver 'GridChordName cause))
            ;; This logic is similar to Chord_name_engraver.
            (let* ((grob-text (ly:grob-property chord-name 'text #f))
                   (final-text (or grob-text text)))
              (ly:grob-set-property! chord-name 'text final-text))))))
     (acknowledgers
      ((bar-line-interface engraver grob source-engraver)
       (set! left-bound grob)
       (set! bar-found grob)))
     ((stop-translation-timestep engraver)
      (when bar-found
        (for-each
         (lambda (c)
           (ly:spanner-set-bound! c RIGHT bar-found))
         accumulated-chord-names)
        (set! accumulated-chord-names '())
        (set! bar-found #f))
      (when chord-name
        (ly:spanner-set-bound! chord-name LEFT left-bound)
        (set! accumulated-chord-names (cons chord-name accumulated-chord-names))
        (set! chord-name #f)))
     ((finalize engraver)
      ;; Maybe the piece ended without a bar line.
      (let ((column (ly:context-property context 'currentCommandColumn)))
        (for-each
         (lambda (c)
           (ly:spanner-set-bound! c RIGHT column))
         accumulated-chord-names))))))

(ly:register-translator
 Grid_chord_name_engraver 'Grid_chord_name_engraver
 '((grobs-created . (GridChordName))
   (events-accepted . ())
   (properties-read . (currentChordText currentChordCause currentCommandColumn))
   (properties-written . ())
   (description . "Read @code{currentChordText} to create chord names
adapted for typesetting within a chord grid.")))
