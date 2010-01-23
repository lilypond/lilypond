%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2008--2010 Carl D. Sorensen <c_sorensen@byu.edu>
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

\version "2.12.0"

% chord-shape-table is a hash-table of chord shapes
% in the form of diagram-descriptions that can be
% fret-diagram-verbose markup-llsts or
% fret-diagram-terse strings.
% The hash keys are pairs of scheme symbols and
% string tunings.  For convenience, the symbols in
% this file are LilyPond chordmode chord descriptions,
% but that is unnecessary.

% music function for adding a chord shape to
% chord-shape-table

addChordShape =
#(define-music-function (parser location key-symbol tuning shape-definition)
   (symbol? pair? string-or-pair?)
   (_i "Add chord shape @var{shape-definition} to the @var{chord-shape-table}
hash with the key @var{(cons key-symbol tuning)}.")
   (hash-set! chord-shape-table
               (cons key-symbol tuning)
               shape-definition)
   (make-music 'SequentialMusic 'void #t))

#(define (chord-shape shape-code tuning)
   (get-chord-shape shape-code tuning chord-shape-table))

% music function for adding a predefined diagram to
% fretboard-table

storePredefinedDiagram =
#(define-music-function (parser location chord tuning diagram-definition)
  (ly:music? pair? string-or-pair?)
  (_i "Add predefined fret diagram defined by @var{diagram-definition}
  for the chord pitches @var{chord} and the stringTuning @var{tuning}.")
  (let* ((pitches (event-chord-pitches 
                    (car (extract-named-music chord 'EventChord))))
         (hash-key (cons tuning pitches))
         (verbose-definition (if (string? diagram-definition)
                                 (parse-terse-string diagram-definition)
                                 diagram-definition)))
  (hash-set! fretboard-table 
             hash-key 
             verbose-definition))
  (make-music 'SequentialMusic 'void #t))
