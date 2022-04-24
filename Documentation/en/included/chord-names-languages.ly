\version "2.16.0"
\header  {
    texidoc = "@cindex Chord Names German
The English naming of chords (default) can be changed to German 
(@code{\germanChords} replaces B and Bes to H and B), semi-German 
(@code{\semiGermanChords} replaces B and Bes to H and Bb), Italian
(@code{\italianChords} uses Do Re Mi Fa Sol La Si), or French
(@code{\frenchChords} replaces Re to Ré).

" }

scm = \chordmode {
    e1/d c:m
    % c/c cis/cis
    % yeah, we get the idea. -hwn
    
    % cisis/cisis ces/ces ceses/ceses
    b/b bis/bis bes/bes
    % beses/beses
} 


\layout {
    ragged-right = ##t 
    \context {\ChordNames \consists Instrument_name_engraver }
}

<<
    \new ChordNames {
	\set instrumentName = "default"
	\scm
    }
    \new ChordNames {
	\set instrumentName = "german"
	\germanChords \scm }
    \new ChordNames {
	\set instrumentName = "semi-german"
	\semiGermanChords \scm }
    \new ChordNames {
	\set instrumentName = "italian"
	\italianChords \scm }
    \new ChordNames {
	\set instrumentName = "french"
	\frenchChords \scm }

    \context Voice { \scm }
>>
