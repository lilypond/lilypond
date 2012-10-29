\version "2.17.6"

\header {
  texidoc = "Multi measure rest staff position can be overridden
to 0.
"
}

\relative c' {
  \override MultiMeasureRest.staff-position = #0
  R1
}
