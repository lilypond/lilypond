

\version "2.1.26"
\header{
texidoc="
A square bracket on the left indicates that the player should not
arpeggiate the chord.
"
}

\score{
     \notes\relative c''{
	 \override Staff.Arpeggio  #'print-function = \arpeggioBracket
	 
	 <fis,  d a >\arpeggio
	 }
     
    \paper { raggedright= ##t }
}

