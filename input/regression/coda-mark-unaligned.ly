\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Where a coda mark is not aligned on a measure boundary,
the bar line defined by @code{underlyingRepeatType} appears by
default.  In this case, the single coda sign should have a normal bar
line and the double coda sign should have a dotted bar line."
}

\new Score \with { underlyingRepeatType = ";" } {
  r4 \codaMark \default \bar "|" % override underlyingRepeatType
  r4 r4 \codaMark \default
  r4
}
