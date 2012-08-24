\layout { ragged-right= ##t }


\version "2.16.0"
\header{
  texidoc="
A square bracket on the left indicates that the player should not
arpeggiate the chord.
"
}

\relative c''{
  \arpeggioBracket
  
  <fis,  d a >\arpeggio
}


