\version "2.27.0"

\header {
  texidoc = "This tests an accelerando followed immediately by a ritardando."
}

#(ly:set-option 'warning-as-error #t)

\score {
  {
    \tempo 4 = 120
    \startGradualTempoChange \default
    \*8 c4
    \tempo 4 = 240
    \startGradualTempoChange \default
    \*8 c4
    \tempo 4 = 60
    c1
  }
  \midi { }
}
