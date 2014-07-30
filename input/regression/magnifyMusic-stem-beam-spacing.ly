\version "2.19.8"

\header {
  texidoc = "Stem length/thickness, beam spacing/thickness, and
horizontal spacing should be scaled along with notation size when
using the @code{\magnifyMusic} command.  Stems can get thicker
than the default, but not thinner."
}

\score {
  \new Voice {
    \omit Staff.TimeSignature
    \time 7/8
    <<
      { \repeat unfold 7 \relative { g'32[ a b c] } }
      {
        \magnifyMusic 0.50 s8
        \magnifyMusic 0.63 s
        \magnifyMusic 0.80 s
        \magnifyMusic 1.00 s
        \magnifyMusic 1.26 s
        \magnifyMusic 1.59 s
        \magnifyMusic 2.00 s
      }
    >>
  }
  \addlyrics {
    "50%" _ _ _ _ _ _ _ _ _ _ _
    "100%" _ _ _ _ _ _ _ _ _ _ _
    "200%" _ _ _
  }
}
