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

(use-modules (ice-9 match))

(define (Chord_square_engraver context)
  (let (;; Current ChordSquare grob.
        (square #f)
        ;; List of moments at which the measure is split (triggered by chords or
        ;; rests or skips).  Used to determine measure-division property.
        (moms '())
        ;; Whether this is the first time step.
        (first-timestep #t)
        ;; Chord name found.  Expect only one chord name per time step.
        (chord-name #f)
        ;; Whether we are in a skip.
        (in-skip #f)
        ;; Moment at which the latest chord stops playing.
        (chord-end-mom (ly:make-moment -inf.0))
        ;; Whether the ChordSquare was created at this time step.
        (new-square-created #f)
        ;; Index for the next chord name we will find.  Reset after terminating
        ;; a ChordSquare.
        (index 0))
    (define (set-square-measure-division!)
      (let* ((now-mom (ly:context-current-moment context))
             (extended-moms (cons now-mom moms))
             (mom-deltas (let loop ((remaining extended-moms)
                                    (acc '()))
                           (match remaining
                             ((_)
                              acc)
                             ((m1 m2 . rest)
                              (loop (cons m2 rest)
                                    (cons (ly:moment-sub m1 m2)
                                          acc))))))
             (total-span (ly:moment-sub now-mom (last extended-moms)))
             (fracs (map (lambda (delta)
                           ;; Don't care about grace chord names.
                           (ly:moment-main (ly:moment-div delta total-span)))
                         mom-deltas)))
        (ly:grob-set-property! square 'measure-division fracs)))
    (make-engraver
     ((start-translation-timestep engraver)
      ;; Can't be done in stop-translation-timestep since the value
      ;; needs to be known in finalize.
      (set! new-square-created #f))
     ((process-music engraver)
      (when first-timestep
        (let ((column (ly:context-property context 'currentCommandColumn)))
          (set! square (ly:engraver-make-grob engraver 'ChordSquare '()))
          (ly:spanner-set-bound! square LEFT column))
        (set! new-square-created #t)))
     (acknowledgers
      ((bar-line-interface engraver grob source-engraver)
       (when (not first-timestep) ; don't duplicate if there's a bar line at the beginning
         (let ((column (ly:context-property context 'currentCommandColumn)))
           (ly:spanner-set-bound! square RIGHT grob)
           (set-square-measure-division!)
           (set! moms '())
           ;; Reset so skips in the next measure count as a new part of a square.
           (set! in-skip #f)
           (ly:engraver-announce-end-grob engraver square grob)
           (set! square (ly:engraver-make-grob engraver 'ChordSquare '()))
           (ly:spanner-set-bound! square LEFT grob)
           (set! new-square-created #t)
           (set! index 0)))
       (when first-timestep
         ;; If we do have a bar line at the beginning, make it the bound of the
         ;; already created chord square.
         (ly:spanner-set-bound! square LEFT grob)))
      ((grid-chord-name-interface engraver grob source-engraver)
       (set! chord-name grob)))
     ((stop-translation-timestep engraver)
      (let ((mom (ly:context-current-moment context)))
        (cond
         ;; Chord name found: register it.
         (chord-name
          (set! moms (cons mom moms))
          (ly:pointer-group-interface::add-grob square 'chord-names chord-name)
          (ly:grob-set-property! chord-name 'index index)
          (ly:grob-set-parent! chord-name X square)
          (ly:grob-set-parent! chord-name Y square)
          (set! index (1+ index))
          (let* ((ev (event-cause chord-name))
                 (length (ly:event-property ev 'length))
                 (now (ly:context-current-moment context)))
            (set! chord-end-mom (ly:moment-add now length)))
          (set! chord-name #f)
          (set! in-skip #f))
         ;; No chord name found and previous chord name no longer active.  This
         ;; is a skip or something similar.  Record the moment where the skip
         ;; started -- unless it already started earlier, in which case it is
         ;; merged with the previous one (also happens if another voice has
         ;; material that causes time steps in the middle of the skip in the
         ;; chord grid).
         ((and (or (not chord-end-mom)
                   (let ((now (ly:context-current-moment context)))
                     (moment<=? chord-end-mom now)))
               (not in-skip))
          (set! moms (cons mom moms))
          (set! in-skip #t)
          (set! index (1+ index)))))
      (set! first-timestep #f))
     ((finalize engraver)
      ;; Avoid a dangling ChordSquare at the end.  If it was created in this
      ;; time step, the music ends on a bar line, as most music does, or perhaps
      ;; the music has zero length.  In that case, kill it silently.  Else, we
      ;; have an unterminated measure, so warn about it.
      (when (not new-square-created)
        (ly:warning (G_ "unterminated measure for chord square")))
      (let ((chord-names (ly:grob-object square 'chord-names #f)))
        (when chord-names
          (for-each ly:grob-suicide! (ly:grob-array->list chord-names))))
      (ly:grob-suicide! square)))))

(ly:register-translator
 Chord_square_engraver 'Chord_square_engraver
 '((events-accepted . ())
   (grobs-created . (ChordSquare))
   (properties-read . (currentCommandColumn))
   (properties-written . ())
   (description . "Engrave chord squares in chord grids.")))
