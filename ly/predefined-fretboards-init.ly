%%%% predefined-fretboard-init.ly
%%%%
%%%% source file of the GNU LilyPond music typesetter
%%%%
%%%% (c) 2008--2009 Carl D. Sorensen <c_sorensen@byu.edu>

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
