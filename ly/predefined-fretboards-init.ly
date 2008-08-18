%%%% predefined-fretboard-init.ly
%%%%
%%%% source file of the GNU LilyPond music typesetter
%%%%
%%%% (c) 2008 Carl D. Sorensen <c_sorensen@byu.edu>

\version "2.11.56"

%%%%% define storage structures

% base-chord-shapes is an alist of chord shapes
% in the form of fret-diagram-terse strings with
% scheme symbols as keys.  For convenience, the
% symbols are LilyPond chordmode chord descriptions,
% but that is unnecessary.

#(define base-chord-shapes '())


% music function for adding a chord shape to
% base-chord-shapes

addChordShape =
#(define-music-function (parser location key-symbol shape-string)
   (symbol? string?)
   (set! base-chord-shapes 
           (acons key-symbol shape-string base-chord-shapes))
   (make-music 'SequentialMusic 'void #t))

% for convenience, to eliminate storage list in .ly references

#(define (chord-shape shape-code)
   (get-chord-shape shape-code base-chord-shapes))

% music function for adding a predefined diagram to
% fretboard-table

storePredefinedDiagram =
#(define-music-function (parser location chord tuning terse-definition)
  (ly:music? list? string?)
  (let* ((pitches (event-chord-pitches 
                    (car (extract-named-music chord 'EventChord))))
         (hash-key (cons tuning pitches)))
  (hash-set! fretboard-table 
             hash-key 
             (parse-terse-string terse-definition)))
  (make-music 'SequentialMusic 'void #t))

