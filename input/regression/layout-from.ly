\version "2.19.21"

\header {
  texidoc = "
Inside of output definitions like @code{\\layout} or @code{\\midi},
music is harvested for layout definitions in order to turn them
into context modifications.
"
}

\score {
  \relative { cis' cis cis cis }
  \layout {
    \accidentalStyle dodecaphonic
  }
  \midi {
    \tempo 4 = 240
  }
}
