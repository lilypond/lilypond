\version "2.27.2"

\header {
  texidoc = "@code{\\time} and @code{\\setMeasureLengthFromHere} take effect in
sequence.  The output should be a complete, irregular measure containing only
two quarter notes even though the time signature is 3/4."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #all-visible
} {
  \time 3/4
  \setMeasureLengthFromHere 2
  c'4 4 |
}
