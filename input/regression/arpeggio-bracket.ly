

\version "2.1.21"
\header{
texidoc="
A square bracket on the left indicates that the player should not
arpeggiate the chord.
"
}

\score{
     \notes\relative c''{
	 \property Staff.Arpeggio \override #'print-function = \arpeggioBracket
	 
	 <fis,  d a >\arpeggio
	 }
     
    \paper { raggedright= ##t }
}

