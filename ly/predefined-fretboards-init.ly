%%%% predefined-fretboard-init.ly
%%%%
%%%% source file of the GNU LilyPond music typesetter
%%%%
%%%% (c) 2008 Carl D. Sorensen <c_sorensen@byu.edu>

%%%%% define storage structures

% base-chord-shapes is an alist of chord shapes
% in the form of fret-diagram-terse strings with
% scheme symbols as keys.  For convenience, the
% symbols are LilyPond chordmode chord descriptions,
% but that is unnecessary.

#(define base-chord-shapes
  (list
    '(f . "1-1-(;3-3;3-4;2-2;1-1;1-1-);")
    '(f:m . "1-1-(;3-3;3-4;1-1;1-1;1-1-);")
    '(f:7 . "1-1-(;3-3;1-1;2-2;1-1;1-1-);")
    '(f:m7 . "1-1-(;3-3;1-1;1-1;1-1;1-1-);")
    '(bes . "x;1-1-(;3-2;3-3;3-4;1-1-);")
    '(bes:m . "x;1-1-(;3-3;3-4;2-2;1-1-);")
    '(bes:m7 . "x;1-1-(;3-3;1-1;2-2;1-1-);")
    '(bes:7 . "x;1-1-(;3-2;1-1;3-3;1-1-);")
    '(bes:maj7 . "x;1-1;3-3;2-2;3-4;x;")
    '(c:dim . "x;x;1-1;2-3;1-2;2-4;")
    '(c:aug . "x;x;2-2;1-1-(;1-1-);4-4;")
    '(cis . "x;x;3-3;1-1-(;2-2;1-1-);")
    '(ees:dim . "x;x;1-1;2-3;1-2;2-4;")
    '(a:dim . "x;x;1-1;2-3;1-2;2-4;")
  ))


% music function for adding a chord shape to
% base-chord-shapes

addChordShape =
#(define-music-function (parser location shape-pair)
   (pair?)
   (set! base-chord-shapes (acons
                             (car shape-pair)
                             (cdr shape-pair)
                             base-chord-shapes))
   (make-music 'SequentialMusic 'void #t))


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


% for convenience, to eliminate storage list in .ly references

#(define (chord-shape shape-code)
   (get-chord-shape shape-code base-chord-shapes))

#(define predefined-diagram-table fretboard-table)

%%%%%%%%%%%%%%%%%%%%%%%
%%%

% definitions of predefined diagrams below here

%%%%  c chords
%
\storePredefinedDiagram \chordmode {c}
                        #guitar-tuning 
                        #"x;3-3;2-2;o;1-1;o;"  
\storePredefinedDiagram \chordmode {c:m} 
                        #guitar-tuning 
                        #(offset-fret 2 (chord-shape 'bes:m))
\storePredefinedDiagram \chordmode {c:aug} 
                        #guitar-tuning 
                        #(chord-shape 'c:aug)
\storePredefinedDiagram \chordmode {c:dim} 
                        #guitar-tuning 
                        #(chord-shape 'c:dim)
\storePredefinedDiagram \chordmode {c:7} 
                        #guitar-tuning 
                        #"o;3-3;2-2;3-4;1-1;o;"  
\storePredefinedDiagram \chordmode {c:maj7} 
                        #guitar-tuning 
                        #"x;3-3;2-2;o;o;o;"  
\storePredefinedDiagram \chordmode {c:m7} 
                        #guitar-tuning 
                        #(offset-fret 2 (chord-shape 'bes:m7))

%%%%  cis chords
%
\storePredefinedDiagram \chordmode {cis} 
                        #guitar-tuning 
                        #(chord-shape 'cis)
\storePredefinedDiagram \chordmode {cis:m} 
                        #guitar-tuning 
                        #"x;x;2-2;1-1;2-3;o;"  
\storePredefinedDiagram \chordmode {cis:aug} 
                        #guitar-tuning 
                        #"x;4-4;3-3;2-1;2-2;x;"  
\storePredefinedDiagram \chordmode {cis:dim} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'c:dim))
\storePredefinedDiagram \chordmode {cis:7} 
                        #guitar-tuning 
                        #"x;x;3-2;4-3;2-1;4-4;"  
\storePredefinedDiagram \chordmode {cis:maj7} 
                        #guitar-tuning 
                        #"x;4-4;3-3;1-1-(;1-1;1-1-);"  
\storePredefinedDiagram \chordmode {cis:m7} 
                        #guitar-tuning 
                        #"x;4-4;2-2;1-1;o;o;"

%%%%  des chords
%
\storePredefinedDiagram \chordmode {des} 
                        #guitar-tuning 
                        #"x;x;3-3;1-1-(;2-2;1-1-);"  
\storePredefinedDiagram \chordmode {des:m} 
                        #guitar-tuning 
                        #"x;x;2-2;1-1;2-3;o;"  
\storePredefinedDiagram \chordmode {des:aug} 
                        #guitar-tuning 
                        #"x;4-4;3-3;2-1;2-2;x;"  
\storePredefinedDiagram \chordmode {des:dim} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'c:dim))
\storePredefinedDiagram \chordmode {des:7} 
                        #guitar-tuning 
                        #"x;x;3-2;4-3;2-1;4-4;"  
\storePredefinedDiagram \chordmode {des:maj7} 
                        #guitar-tuning 
                        #"x;4-4;3-3;1-1-(;1-1;1-1-);"  
\storePredefinedDiagram \chordmode {des:m7} 
                        #guitar-tuning 
                        #"x;4-4;2-2;1-1;o;o;"

%%%%  d chords
%
\storePredefinedDiagram \chordmode {d} 
                        #guitar-tuning 
                        #"x;x;o;2-1;3-2;2-3;"  
\storePredefinedDiagram \chordmode {d:m} 
                        #guitar-tuning 
                        #"x;x;o;2-2;3-3;1-1;"  
\storePredefinedDiagram \chordmode {d:aug} 
                        #guitar-tuning 
                        #"x;x;o;3-2;3-3;2-1;"  
\storePredefinedDiagram \chordmode {d:dim} 
                        #guitar-tuning 
                        #"x;x;o;1-1;o;1-2;"  
\storePredefinedDiagram \chordmode {d:7} 
                        #guitar-tuning 
                        #"x;x;o;2-2;1-1;2-3;"  
\storePredefinedDiagram \chordmode {d:maj7} 
                        #guitar-tuning 
                        #"x;x;o;2-1;2-2;2-3;"  
\storePredefinedDiagram \chordmode {d:m7} 
                        #guitar-tuning 
                        #"x;x;o;2-2;1-1-(;1-1-);"  

%%%%  dis chords
%
\storePredefinedDiagram \chordmode {dis} 
                        #guitar-tuning 
                        #(offset-fret 2 (chord-shape 'cis))
\storePredefinedDiagram \chordmode {dis:m} 
                        #guitar-tuning 
                        #"x;x;4-3;3-2;4-4;1-1;"  
\storePredefinedDiagram \chordmode {dis:aug} 
                        #guitar-tuning 
                        #"3-3;2-2;1-1;o;o;3-4"  
\storePredefinedDiagram \chordmode {dis:dim} 
                        #guitar-tuning 
                        #(chord-shape 'ees:dim) 
\storePredefinedDiagram \chordmode {dis:7} 
                        #guitar-tuning 
                        #"x;x;1-1;3-3;2-2;3-4;"  
\storePredefinedDiagram \chordmode {dis:maj7} 
                        #guitar-tuning 
                        #"x;x;1-1;3-2;3-3;3-4;"  
\storePredefinedDiagram \chordmode {dis:m7} 
                        #guitar-tuning 
                        #"x;x;1-1;3-2;2-3;2-4;"  

%%%%  ees chords
%
\storePredefinedDiagram \chordmode {ees} 
                        #guitar-tuning 
                        #(offset-fret 2 (chord-shape 'cis))
\storePredefinedDiagram \chordmode {ees:m} 
                        #guitar-tuning 
                        #"x;x;4-3;3-2;4-4;1-1;"  
\storePredefinedDiagram \chordmode {ees:aug} 
                        #guitar-tuning 
                        #"3-3;2-2;1-1;o;o;3-4"  
\storePredefinedDiagram \chordmode {ees:dim} 
                        #guitar-tuning 
                        #(chord-shape 'ees:dim) 
\storePredefinedDiagram \chordmode {ees:7} 
                        #guitar-tuning 
                        #"x;x;1-1;3-3;2-2;3-4;"  
\storePredefinedDiagram \chordmode {ees:maj7} 
                        #guitar-tuning 
                        #"x;x;1-1;3-2;3-3;3-4;"  
\storePredefinedDiagram \chordmode {ees:m7} 
                        #guitar-tuning 
                        #"x;x;1-1;3-2;2-3;2-4;"  

%%%%  e chords
%
\storePredefinedDiagram \chordmode {e} 
                        #guitar-tuning 
                        #"o;2-2;2-3;1-1;o;o;"  
\storePredefinedDiagram \chordmode {e:m} 
                        #guitar-tuning 
                        #"o;2-2;2-3;o;o;o;"  
\storePredefinedDiagram \chordmode {e:aug} 
                        #guitar-tuning 
                        #"o;3-3;2-2;1-1;x;x;"  
\storePredefinedDiagram \chordmode {e:dim} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'ees:dim))
\storePredefinedDiagram \chordmode {e:7} 
                        #guitar-tuning 
                        #"o;2-2;o;1-1;o;o;"  
\storePredefinedDiagram \chordmode {e:maj7} 
                        #guitar-tuning 
                        #"o;2-3;1-1;1-2;o;x;"  
\storePredefinedDiagram \chordmode {e:m7} 
                        #guitar-tuning 
                        #"o;2-2;o;o;o;o;"  

%%%%  f chords
%
\storePredefinedDiagram \chordmode {f} 
                        #guitar-tuning 
                        #(chord-shape 'f)
\storePredefinedDiagram \chordmode {f:m} 
                        #guitar-tuning 
                        #(chord-shape 'f:m)
\storePredefinedDiagram \chordmode {f:aug} 
                        #guitar-tuning 
                        #"x;x;1-1;4-3;4-4;3-2;"  
\storePredefinedDiagram \chordmode {f:dim} 
                        #guitar-tuning 
                        #"x;x;o;1-1;o;1-2;"  
\storePredefinedDiagram \chordmode {f:7} 
                        #guitar-tuning 
                        #(chord-shape 'f:7)
\storePredefinedDiagram \chordmode {f:maj7} 
                        #guitar-tuning 
                        #"x;3-3;3-4;2-2;1-1;"  
\storePredefinedDiagram \chordmode {f:m7} 
                        #guitar-tuning 
                        #(chord-shape 'f:m7)

%%%%  fis chords
%
\storePredefinedDiagram \chordmode {fis} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'f))
\storePredefinedDiagram \chordmode {fis:m} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'f:m))
\storePredefinedDiagram \chordmode {fis:aug} 
                        #guitar-tuning 
                        #"2-2;1-1;o;3-4-(;3-4-);2-3;" 
\storePredefinedDiagram \chordmode {fis:dim} 
                        #guitar-tuning 
                        #"x;x;1-1;2-3;1-2;2-4;"  
\storePredefinedDiagram \chordmode {fis:7} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'f:7))
\storePredefinedDiagram \chordmode {fis:maj7} 
                        #guitar-tuning 
                        #"x;x;4-4;3-3;2-2;1-1;"  
\storePredefinedDiagram \chordmode {fis:m7} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'f:m7))

%%%%  ges chords
%
\storePredefinedDiagram \chordmode {ges} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'f))
\storePredefinedDiagram \chordmode {ges:m} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'f:m))
\storePredefinedDiagram \chordmode {ges:aug} 
                        #guitar-tuning 
                        #"2-2;1-1;o;3-4-(;3-4-);2-3;" 
\storePredefinedDiagram \chordmode {ges:dim} 
                        #guitar-tuning 
                        #"x;x;1-1;2-3;1-2;2-4;"  
\storePredefinedDiagram \chordmode {ges:7} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'f:7))
\storePredefinedDiagram \chordmode {ges:maj7} 
                        #guitar-tuning 
                        #"x;x;4-4;3-3;2-2;1-1;"  
\storePredefinedDiagram \chordmode {ges:m7} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'f:m7))

%%%%  g chords
%
\storePredefinedDiagram \chordmode {g} 
                        #guitar-tuning 
                        #"3-2;2-1;o;o;o;3-3;"  
\storePredefinedDiagram \chordmode {g:m} 
                        #guitar-tuning 
                        #(offset-fret 2 (chord-shape 'f:m))
\storePredefinedDiagram \chordmode {g:aug} 
                        #guitar-tuning 
                        #"x;x;5-1;8-3;8-4;7-2;"  
\storePredefinedDiagram \chordmode {g:dim} 
                        #guitar-tuning 
                        #"x;x;5-2;6-4;5-3;3-1;"  
\storePredefinedDiagram \chordmode {g:7} 
                        #guitar-tuning 
                        #"3-3;2-2;o;o;o;1-1;"  
\storePredefinedDiagram \chordmode {g:maj7} 
                        #guitar-tuning 
                        #"x;x;5-4;4-3;3-2;2-1;"  
\storePredefinedDiagram \chordmode {g:m7} 
                        #guitar-tuning 
                        #(offset-fret 2 (chord-shape 'f:m7))

%%%%  gis chords
%
\storePredefinedDiagram \chordmode {gis} 
                        #guitar-tuning 
                        #(offset-fret 3 (chord-shape 'f))
\storePredefinedDiagram \chordmode {gis:m} 
                        #guitar-tuning 
                        #(offset-fret 3 (chord-shape 'f:m))
\storePredefinedDiagram \chordmode {gis:aug} 
                        #guitar-tuning 
                        #"o;3-4;2-3;1-1;1-2;o;"  
\storePredefinedDiagram \chordmode {gis:dim} 
                        #guitar-tuning 
                        #"x;x;o;1-1;o;1-2;"  
\storePredefinedDiagram \chordmode {gis:7} 
                        #guitar-tuning 
                        #(offset-fret 3 (chord-shape 'f:7))
\storePredefinedDiagram \chordmode {gis:maj7} 
                        #guitar-tuning 
                        #"x;x;1-1-(;1-1;1-1-);3-3;"  
\storePredefinedDiagram \chordmode {gis:m7} 
                        #guitar-tuning 
                        #(offset-fret 3 (chord-shape 'f:m7))

%%%%  aes chords
%
\storePredefinedDiagram \chordmode {aes} 
                        #guitar-tuning 
                        #(offset-fret 3 (chord-shape 'f))
\storePredefinedDiagram \chordmode {aes:m} 
                        #guitar-tuning 
                        #(offset-fret 3 (chord-shape 'f:m))
\storePredefinedDiagram \chordmode {aes:aug} 
                        #guitar-tuning 
                        #"o;3-4;2-3;1-1;1-2;o;"  
\storePredefinedDiagram \chordmode {aes:dim} 
                        #guitar-tuning 
                        #"x;x;o;1-1;o;1-2;"  
\storePredefinedDiagram \chordmode {aes:7} 
                        #guitar-tuning 
                        #(offset-fret 3 (chord-shape 'f:7))
\storePredefinedDiagram \chordmode {aes:maj7} 
                        #guitar-tuning 
                        #"x;x;1-1-(;1-1;1-1-);3-3;"  
\storePredefinedDiagram \chordmode {aes:m7} 
                        #guitar-tuning 
                        #(offset-fret 3 (chord-shape 'f:m7))

%%%%  a chords
%
\storePredefinedDiagram \chordmode {a} 
                        #guitar-tuning 
                        #"x;o;2-1;2-2;2-3;o;"  
\storePredefinedDiagram \chordmode {a:m} 
                        #guitar-tuning 
                        #"x;o;2-2;2-3;1-1;o;"  
\storePredefinedDiagram \chordmode {a:aug} 
                        #guitar-tuning 
                        #"x;o;3-4;2-2;2-3;1-1;"  
\storePredefinedDiagram \chordmode {a:dim} 
                        #guitar-tuning 
                        #(chord-shape 'a:dim)
\storePredefinedDiagram \chordmode {a:7} 
                        #guitar-tuning 
                        #"x;o;2-1;o;2-3;o;"  
\storePredefinedDiagram \chordmode {a:maj7} 
                        #guitar-tuning 
                        #"x;o;2-2;1-1;2-3;o;"  
\storePredefinedDiagram \chordmode {a:m7} 
                        #guitar-tuning 
                        #"x;o;2-2;o;1-1;o;"  

%%%%  ais chords
%
\storePredefinedDiagram \chordmode {ais} 
                        #guitar-tuning 
                        #(chord-shape 'bes)
\storePredefinedDiagram \chordmode {ais:m} 
                        #guitar-tuning 
                        #(chord-shape 'bes:m)
\storePredefinedDiagram \chordmode {ais:aug} 
                        #guitar-tuning 
                        #"2-2;1-1;o;3-4-(;3-4-);2-3;"  
\storePredefinedDiagram \chordmode {ais:dim} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'a:dim))
\storePredefinedDiagram \chordmode {ais:7} 
                        #guitar-tuning 
                        #(chord-shape 'bes:7)
\storePredefinedDiagram \chordmode {ais:maj7} 
                        #guitar-tuning 
                        #"x;1-1;3-3;2-2;3-4;x;"  
\storePredefinedDiagram \chordmode {ais:m7} 
                        #guitar-tuning 
                        #(chord-shape 'bes:m7)

%%%%  bes chords
%
\storePredefinedDiagram \chordmode {bes} 
                        #guitar-tuning 
                        #(chord-shape 'bes)
\storePredefinedDiagram \chordmode {bes:m} 
                        #guitar-tuning 
                        #(chord-shape 'bes:m)
\storePredefinedDiagram \chordmode {bes:aug} 
                        #guitar-tuning 
                        #"2-2;1-1;o;3-4-(;3-4-);2-3;"  
\storePredefinedDiagram \chordmode {bes:dim} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'a:dim))
\storePredefinedDiagram \chordmode {bes:7} 
                        #guitar-tuning 
                        #(chord-shape 'bes:7)
\storePredefinedDiagram \chordmode {bes:maj7} 
                        #guitar-tuning 
                        #"x;1-1;3-3;2-2;3-4;x;"  
\storePredefinedDiagram \chordmode {bes:m7} 
                        #guitar-tuning 
                        #(chord-shape 'bes:m7)

%%%%  b chords
%
\storePredefinedDiagram \chordmode {b} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'bes))
\storePredefinedDiagram \chordmode {b:m} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'bes:m))
\storePredefinedDiagram \chordmode {b:aug} 
                        #guitar-tuning 
                        #"x;3-2;2-1;o;o;x;"  
\storePredefinedDiagram \chordmode {b:dim} 
                        #guitar-tuning 
                        #"x;x;o;1-1;o;1-2;"  
\storePredefinedDiagram \chordmode {b:7} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'bes:7))
\storePredefinedDiagram \chordmode {b:maj7} 
                        #guitar-tuning 
                        #"x;2-1;4-3;3-2;4-4;x;"  
\storePredefinedDiagram \chordmode {b:m7} 
                        #guitar-tuning 
                        #(offset-fret 1 (chord-shape 'bes:m7))

