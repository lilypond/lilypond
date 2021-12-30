\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="When @code{\\jump} is at a line break, the text appears at
the end of the line."
}

\fixed c' {
  R1*2
  \break \jump "Go get coffee"
  R1*2
}
