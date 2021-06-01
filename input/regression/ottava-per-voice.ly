\version "2.23.3"

\header {
  texidoc = "Ottava brackets can be made to apply to a single voice
by moving the @code{Ottava_spanner_engraver} to @code{Voice} context."
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
