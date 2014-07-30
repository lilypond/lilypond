\version "2.19.8"

\header {
  texidoc = "Slurs should be scaled along with notation size when
using the @code{\magnifyMusic} command.  They can get thicker than
the default, but not thinner."
}

\score {
  \new Voice {
    \omit Staff.TimeSignature
    \time 7/8
    <<
      { \repeat unfold 7 \relative { g'32[( a b c)] } }
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
  \addlyrics { "50%" _ _ "100%" _ _ "200%" }
}
