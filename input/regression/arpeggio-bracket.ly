

\version "2.4.0"
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
     
    \layout { raggedright= ##t }
}

