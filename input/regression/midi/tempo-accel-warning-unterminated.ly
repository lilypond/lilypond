\version "2.27.0"

\header {
  texidoc = "An unterminated gradual tempo change triggers a warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "unterminated tempo change"))

\score {
  \fixed c' { \startGradualTempoChange \default \*3 c1 }
  \midi { }
}
