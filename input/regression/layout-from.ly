\version "2.15.32"

\header {
  texidoc = "
Inside of output definitions like @code{\\layout} or @code{\\midi},
music is harvested for layout definitions in order to turn them
into context modifications.
"
}

\score {
  \relative c' { cis cis cis cis }
  \layout {
    \accidentalStyle "dodecaphonic"
  }
  \midi {
    \tempo 4 = 240
  }
}
