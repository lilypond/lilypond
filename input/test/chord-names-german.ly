\version "2.1.22"
\header  {
    texidoc = "@cindex Chord Names German
By setting @code{chordRootNamer}, the root
of the chord may be named with a different function.

Setting @code{\germanChords} gives true german chord-names,
@code{\semiGermanChords} gives semi-german chord-names - - with Bb and
keeping the english names.


" }

scm = \chords {
    c1/c cis/cis
    % yeah, we get the idea. -hwn
    
    % cisis/cisis ces/ces ceses/ceses
    b/b bis/bis bes/bes
    % beses/beses
} 
\score {
\notes <<
    \context ChordNames { \scm }
    \new ChordNames {
	\set instrument =  #"german"
	\germanChords \scm }
    \new ChordNames {
	\set instrument = #"semi-german"
	\semiGermanChords \scm }
    \context Voice {  \scm } >>
\paper {
    raggedright = ##t 
    \translator {\ChordNamesContext \consists Instrument_name_engraver }}

}
