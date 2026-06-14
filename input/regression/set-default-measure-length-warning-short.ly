\version "2.27.2"

\header {
  texidoc = "@code{\\setDefaultMeasureLength} warns if the measure position has
already reached the default measure length.

This test should compile with expected warnings.  There are no strict
expectations on the visual output; the current implementation ignores the
missed measure boundaries."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "setting measureLength (%s) ≤ measurePosition (%s)") "1" "1")
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "setting measureLength (%s) ≤ measurePosition (%s)") "1" "5/4")

\new Score \with {
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #all-visible
} \fixed c' {
  \setMeasureLengthFromHere 1.
  c4 c c c
  \setDefaultMeasureLength   % too late: length = pos
  c4 c c c |
  \setMeasureLengthFromHere 1.
  c4 c c c c
  \setDefaultMeasureLength   % too late: length < pos
  c4 c c |
}
