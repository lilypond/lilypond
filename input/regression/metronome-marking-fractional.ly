\version "2.25.23"

\header {
  texidoc = "Non-integer metronome rates are rounded for printing.  The counts
should be 0, 50, 0-50, and@tie{}75-100."
}

tempoA = #1/3
tempoB = #498/10

\fixed c' {
  \tempo 4 = \tempoA
  c1 |
  \tempo 4 = \tempoB
  c1 |
  \tempo 4 = \tempoA-\tempoB
  c1 |
  \tempo 4 = #751/10 -#(inexact->exact 100.1)
  c1 |
}
