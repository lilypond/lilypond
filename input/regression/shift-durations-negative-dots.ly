\version "2.15.21"

\header {

  texidoc = "
  @code{\shiftDurations} can use negative dot values without causing
  a crash.
  "
}

\shiftDurations #1 #-1 { c''1 }

