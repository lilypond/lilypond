\layout { ragged-right= ##t }


\version "2.16.0"
\header{
  texidoc="
A square bracket on the left indicates that the player should not
arpeggiate the chord.
"
}

\relative c' {
  \arpeggioBracket

  <d d>2\arpeggio <d e>\arpeggio
  <d fis>2\arpeggio <d g>\arpeggio
  <d a'>2\arpeggio <d b'>\arpeggio
  <d c'>2\arpeggio <d d'>\arpeggio
}
