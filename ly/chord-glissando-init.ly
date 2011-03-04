%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2010 Carl D. Sorensen <c_sorensen@byu.edu>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.13.49"

%%  Make slide indication for chords
chordGlissando =
#(define-music-function (parser location mus1 mus2) (ly:music? ly:music?)
   "Make a glissando between the notes of triads @code{mus1} and @code{mus2}."

   (define (add-glissando musChord)
     (let ((els (ly:music-property musChord 'elements)))
       (ly:music-set-property! musChord 'elements (append els (list (make-music 'GlissandoEvent))))
       musChord))

   (define (get-notes musicChord)
     (filter (lambda(x) (eq? (ly:music-property x 'name) 'NoteEvent))
             (ly:music-property musicChord 'elements)))

   (define (select-note musChord index)
     (let* ((notes (get-notes musChord))
            (non-notes (filter (lambda (x)
                                 (not (eq? (ly:music-property x 'name)
                                           'NoteEvent)))
                               (ly:music-property musChord 'elements)))
            (selected-note (list-ref notes index))
            (new-els (cons selected-note non-notes))
            (new-mus (ly:music-deep-copy musChord)))
       (ly:music-set-property! new-mus 'elements new-els)
       new-mus))

   (define (add-glissando-line mus1 mus2 index)
     (context-spec-music
      (context-spec-music
       (make-sequential-music
        (list
         hideNotes
         (make-grob-property-set 'StringNumber 'transparent #t)
         (make-grob-property-set 'NoteColumn 'ignore-collision #t)
         ;; obviously, this isn't equivalent to \once,
         ;; so could be changed if required
         (make-grob-property-set 'Glissando 'thickness 2)
         (make-grob-property-set 'DynamicText 'transparent #t)
         (make-grob-property-set 'DynamicLineSpanner 'transparent #t)
         (make-grob-property-set 'DynamicTextSpanner 'transparent #t)
         (add-glissando (select-note mus1 index))
         (select-note mus2 index)))
       'Bottom (symbol->string (gensym)))
      'Staff))

   (let* ((notes1 (get-notes mus1))
          (notes2 (get-notes mus2))
          (note-count (min (length notes1) (length notes2))))

    #{
       \once \override Glissando #'minimum-length = #4
       \once \override Glissando #'springs-and-rods = #ly:spanner::set-spacing-rods
     <<
       {
         $(add-glissando mus1)
         $mus2
       }
       $(make-simultaneous-music
           (map (lambda (x)
                        (add-glissando-line mus1 mus2 x))
                (iota note-count)))
    >>
    \revert NoteColumn #'ignore-collision
  #}))
