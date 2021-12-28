%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2008--2022 Carl D. Sorensen <c_sorensen@byu.edu>
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

\version "2.19.22"

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
#(define-void-function (key-symbol tuning shape-definition)
   (symbol? pair? string-or-pair?)
   (_i "Add chord shape @var{shape-definition} to the @var{chord-shape-table}
hash with the key @code{(cons @var{key-symbol} @var{tuning})}.")
   (hash-set! chord-shape-table
               (cons key-symbol tuning)
               shape-definition))

#(define (chord-shape shape-code tuning)
   (get-chord-shape shape-code tuning chord-shape-table))

% scheme function for copying/creating fretboard tables

#(define (make-fretboard-table . rest)
  "Create a new fretboard table.  @code{rest} is an optional table name.
If present, the new fretboard table starts as a copy of the fretboard
table @code{rest}."
  (if (null? rest)
      (make-hash-table 101)
      (let ((source-table (car rest)))
        (hash-fold
          (lambda (key value tab)
            (hash-set! tab key value)
            tab)
          (make-hash-table 101)
          source-table))))

% music function for adding a predefined diagram to
% fretboard-table

storePredefinedDiagram =
#(define-void-function
   (fretboard-table chord tuning diagram-definition)
   (hash-table? ly:music? pair? string-or-pair?)
  (_i "Add predefined fret diagram defined by @var{diagram-definition}
  for the chord pitches @var{chord} and the stringTuning @var{tuning}.")
  (let* ((pitches (event-chord-pitches
                    (car (extract-named-music chord 'EventChord))))
         (hash-key (cons tuning pitches))
         (verbose-definition (if (string? diagram-definition)
                                 (parse-terse-string
                                   (remove-whitespace diagram-definition))
                                 diagram-definition)))
  (hash-set! fretboard-table
             hash-key
             verbose-definition)))
