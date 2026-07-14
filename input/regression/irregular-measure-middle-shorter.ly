\version "2.27.2"

\header {
  texidoc = "@code{\\measure} can create a shorter-than-expected measure in the
middle of a piece.  The second measure should be beamed @code{2,1}."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #all-visible
} \fixed c' {
  \time 2,4,2 5/8
  \repeat unfold 5 c8
  \measure \repeat unfold 3 d8
  \repeat unfold 5 e8
}
