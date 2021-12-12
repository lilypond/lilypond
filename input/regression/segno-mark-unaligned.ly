\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Where a segno mark is not aligned on a measure boundary,
the bar line defined by @code{underlyingRepeatBarType} appears by
default.  In this case, the single segno should have a normal bar line
and the double segno should have a dotted bar line."
}

\new Score \with { underlyingRepeatBarType = ";" } {
  r4 \segnoMark \default \bar "|" % override underlyingRepeatBarType
  r4 r4 \segnoMark \default
  r4
}
