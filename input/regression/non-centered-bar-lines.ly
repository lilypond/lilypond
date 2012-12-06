\version "2.17.6"
\header {
  texidoc = "Bar lines are positioned correctly when using custom
staves which are not centered around position@tie{}0.
"
}
\new Staff {
  \override Staff.StaffSymbol.line-positions = #'(1 3 5 7 9)
  c''1 \bar "||"
  c''1 \bar ";"
  c''1 \bar "|."
}
