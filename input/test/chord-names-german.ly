\version "2.3.22"
\header  {
    texidoc = "@cindex Chord Names German
The english naming of chords (default) can be changed to german 
(@code{\germanChords} replaces B and Bes to H and B) or semi-german 
(@code{\semiGermanChords} replaces B and Bes to H and Bb).

" }

scm = \chordmode {
    c1/c cis/cis
    % yeah, we get the idea. -hwn
    
    % cisis/cisis ces/ces ceses/ceses
    b/b bis/bis bes/bes
    % beses/beses
} 


\layout {
    raggedright = ##t 
    \context {\ChordNames \consists Instrument_name_engraver }
}

<<
    \new ChordNames {
	\set instrument = #"default"
	\scm
    }
    \new ChordNames {
	\set instrument = #"german"
	\germanChords \scm }
    \new ChordNames {
	\set instrument = #"semi-german"
	\semiGermanChords \scm }
    \context Voice { \scm }
>>
