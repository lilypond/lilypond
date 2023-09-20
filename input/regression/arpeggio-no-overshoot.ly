\version "2.19.21"

\header {
  texidoc = "Arpeggios do not overshoot the highest note head.
The first chord in this example simulates overshoot using
@code{'positions} for comparison with the correct behavior.

Exceptions are intervals smaller than a third; we ensure to have
at least two wiggles (or a wiggle plus an arrow head)."
}

\relative {
  % simulate overshoot for comparison
  \once \override Arpeggio.positions = #'(-3 . 1)
  <c' e g b>1\arpeggio
  <c e g b>1\arpeggio
  <f a c>2\arpeggio <g b d f>\arpeggio
  <f a>2\arpeggio <f g>\arpeggio
  \override Arpeggio.arpeggio-direction = #UP
  <e g>2\arpeggio
  \override Arpeggio.arpeggio-direction = #DOWN
    <e f>\arpeggio
  \revert Arpeggio.arpeggio-direction
  <e e>1\arpeggio
}
