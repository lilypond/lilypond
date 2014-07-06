\version "2.19.8"

\header {
  texidoc = "Dot size and beamlet length should be scaled along
with notation size when using the @code{\magnifyMusic} command."
}

\score {
  \new Voice {
    \omit Staff.TimeSignature
    \time 7/4
    <<
      { \repeat unfold 7 \relative { g'8.[ g16] } }
      {
        \magnifyMusic 0.50 s4
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
    "50%" _ _ _ _ _
    "100%" _ _ _ _ _
    "200%" _
  }
}
