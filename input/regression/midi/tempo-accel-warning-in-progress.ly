\version "2.27.0"

\header {
  texidoc = "Using @code{\\startGradualTempoChange} when a change is already in
progress triggers a warning."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "tempo change already in progress; ignoring"))

\score {
  \fixed c' {
    \tempo 4 = 90
    \startGradualTempoChange \default
    \*4 c4
    \startGradualTempoChange \default  % warning
    \*4 c4
    \stopGradualTempoChange 4 270
    c4
  }
  \midi { }
}
