\version "2.15.31"

\header {
  texidoc = "
@code{\\layout-from} can interpret property-setting music for changing
context definitions inside of layout definitions like @code{\\layout}
or @code{\\midi}.
"
}

\score {
  \relative c' { cis cis cis cis }
  \layout {
    \layout-from { \accidentalStyle "dodecaphonic" }
  }
  \midi {
    \layout-from { \tempo 4 = 240 }
  }
}
