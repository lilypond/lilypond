

\version "2.1.7"
\header{
texidoc="
A square bracket on the left indicates that the player should not
arpeggiate the chord.
"
}

\score{
     \notes\relative c''{
	 \property Staff.Arpeggio \override #'molecule-callback = \arpeggioBracket
	 
	 <fis,  d a >\arpeggio
	 }
     
    \paper { raggedright= ##t }
}

