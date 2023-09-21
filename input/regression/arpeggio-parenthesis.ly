\version "2.16.0"

\header {
  texidoc = "There is a variant of the arpeggio sign that uses a
`vertical slur' instead of the wiggle."
}

\relative c' {
  % Note: does not work for cross-staff arpeggios.
  \arpeggioParenthesis

  <d d>2\arpeggio <d e>\arpeggio
  <d fis>2\arpeggio <d g>\arpeggio
  <d a'>2\arpeggio <d b'>\arpeggio
  <d cis'>2\arpeggio <d d'>\arpeggio
}
