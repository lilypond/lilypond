\version "2.27.2"

\header {
  texidoc = "@code{\\setMeasureLengthFromHere} and @code{\\time} take effect in
sequence.  The output should be exactly one measure in 3/4 time."
}

#(ly:set-option 'warning-as-error #t)

\new Score \with {
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #all-visible
} {
  \setMeasureLengthFromHere 2  % canceled by following \time
  \time 3/4
  c'4 4 4 |
}
