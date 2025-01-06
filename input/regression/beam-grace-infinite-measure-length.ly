\version "2.25.23"

\header {
  texidoc = "Setting @code{Timing@/.measureLength} to infinity does not
interfere with beaming grace notes."
}

#(ly:set-option 'warning-as-error #t)

\fixed c' {
  \set Timing.measureLength = #+inf.0
  c4
  \grace { d8 e8 }
  f4
}
