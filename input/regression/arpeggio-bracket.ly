

\version "2.3.17"
\header{
texidoc="
A square bracket on the left indicates that the player should not
arpeggiate the chord.
"
}

\score{
     \relative c''{
	 \arpeggioBracket
	 
	 <fis,  d a >\arpeggio
	 }
     
    \paper { raggedright= ##t }
}

