\version "2.23.5"

\header {
  texidoc = "Regression test for Issue #6205.  Expected output is a
single staff with notes C and E."
}

{
  c'1
  \tuplet 1/1 \set Timing.baseMoment = #(ly:make-moment 1/4)
  e'1
}
