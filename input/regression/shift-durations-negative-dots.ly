\version "2.16.0"

\header {

  texidoc = "
  @code{\\shiftDurations} can use negative dot values without causing
  a crash.
  "
}

\shiftDurations #1 #-1 { c''1 }

