

\version "2.1.30"
\header{
texidoc="
A square bracket on the left indicates that the player should not
arpeggiate the chord.
"
}

\score{
     \notes\relative c''{
	 \arpeggioBracket
	 
	 <fis,  d a >\arpeggio
	 }
     
    \paper { raggedright= ##t }
}

