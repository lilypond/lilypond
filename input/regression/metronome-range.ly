\version "2.13.41"

\header {
  texidoc = "
Tempo ranges are supported.  By default, numbers are
printed with an en-dash character, separated by thin-spaces.
"
}

\relative c'' {
  \tempo 4 = 66 ~ 72
  c1 c
  \set Score.tempoUnitCount = #(cons 124 132)
  c1 c
}
