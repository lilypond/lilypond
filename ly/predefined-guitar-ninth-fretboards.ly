\version "2.11.61"
%
%  Add ninth chords to to predefined fret diagrams for standard guitar tunings
%
%  This is part of the Lilypond distribution files
%
%  Copyright 2008 by Jonathan Kulp
%
 

\addChordShape #'c:9 #"x;3-2;2-1;3-3-(;3-3;3-3-);"
\addChordShape #'f:9 #"1-1-(;3-3;1-1;2-2;1-1-);3-4;"

\storePredefinedDiagram \chordmode {c:9} 
                        #guitar-tuning 
			#(chord-shape 'c:9)
\storePredefinedDiagram \chordmode {cis:9} 
                        #guitar-tuning 
			#(offset-fret 1 (chord-shape 'c:9))
\storePredefinedDiagram \chordmode {des:9} 
                        #guitar-tuning 
			#(offset-fret 1 (chord-shape 'c:9))
\storePredefinedDiagram \chordmode {d:9} 
                        #guitar-tuning 
			#(offset-fret 2 (chord-shape 'c:9))
\storePredefinedDiagram \chordmode {dis:9} 
                        #guitar-tuning 
			#(offset-fret 3 (chord-shape 'c:9))
\storePredefinedDiagram \chordmode {ees:9} 
                        #guitar-tuning 
			#(offset-fret 3 (chord-shape 'c:9))
\storePredefinedDiagram \chordmode {e:9} 
                        #guitar-tuning 
			#"o;2-2;o;1-1;o;2-3;"
\storePredefinedDiagram \chordmode {f:9} 
                        #guitar-tuning 
			#(chord-shape 'f:9)
\storePredefinedDiagram \chordmode {fis:9} 
                        #guitar-tuning 
			#(offset-fret 1 (chord-shape 'f:9))
\storePredefinedDiagram \chordmode {ges:9} 
                        #guitar-tuning 
			#(offset-fret 1 (chord-shape 'f:9))
\storePredefinedDiagram \chordmode {g:9} 
                        #guitar-tuning 
			#(offset-fret 2 (chord-shape 'f:9))
\storePredefinedDiagram \chordmode {gis:9} 
                        #guitar-tuning 
			#(offset-fret 3 (chord-shape 'f:9))
\storePredefinedDiagram \chordmode {aes:9} 
                        #guitar-tuning 
			#(offset-fret 3 (chord-shape 'f:9))
\storePredefinedDiagram \chordmode {a:9} 
                        #guitar-tuning 
			#(offset-fret 4 (chord-shape 'f:9))
\storePredefinedDiagram \chordmode {ais:9} 
                        #guitar-tuning 
			#(offset-fret 5 (chord-shape 'f:9))
\storePredefinedDiagram \chordmode {bes:9} 
                        #guitar-tuning 
			#(offset-fret 5 (chord-shape 'f:9))
\storePredefinedDiagram \chordmode {b:9} 
                        #guitar-tuning 
			#(offset-fret -1 (chord-shape 'c:9))
