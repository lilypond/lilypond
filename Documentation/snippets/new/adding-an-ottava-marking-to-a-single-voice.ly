\version "2.23.3"

\header {
  lsrtags = "pitches, tweaks-and-overrides"

  texidoc = "
If you have more than one voice on the staff, setting octavation in one
voice transposes the position of notes in all voices for the duration
of the ottava bracket. If the octavation is only intended to apply to
one voice, the @code{Ottava_spanner_engraver} should be moved to
@code{Voice} context.
"

  doctitle = "Adding an ottava marking to a single voice"
}


\layout {
  \context {
    \Staff
    \remove Ottava_spanner_engraver
  }
  \context {
    \Voice
    \consists Ottava_spanner_engraver
  }
}

{
  \clef bass
  << { <g d'>1~ q2 <c' e'> }
  \\
    {
      r2.
      \ottava -1
      <b,,, b,,>4 ~ |
      q2
      \ottava 0
      <c e>2
    }
  >>
}
