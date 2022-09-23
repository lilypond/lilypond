\version "2.19.21"

\header {
  texidoc = "Arpeggios do not overshoot the highest note head.
The first chord in this example simulates overshoot using
@code{'positions} for comparison with the correct behavior."
}

\relative {
  % simulate overshoot for comparison
  \once \override Arpeggio.positions = #'(-3 . 1)
  <c' e g b>1\arpeggio
  <c e g b>1\arpeggio
  <f a c>2\arpeggio <g b d f>\arpeggio
}
