\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Where a @code{\\jump} is not aligned on a measure
boundary, the bar line defined by @code{underlyingRepeatBarType}
appears by default.  In this case, “GOTO 10” should have a normal bar
line and “GOTO 20” should have a dotted bar line."
}

\new Score \with { underlyingRepeatBarType = ";" } {
  r4 r4 r4 \jump "GOTO 10" \bar "|" r4 % override underlyingRepeatBarType
  R1
  r4 r4 r4 \jump "GOTO 20" r4
}
