\version "2.19.21"

\header {
  texidoc = "If using integers, the tie will vertically tuned for
staff line avoidance. If using a floating point number, this is taken
as the exact location."
}

\relative {
  \override Tie.staff-position = #3
  d''4 ~
  \override Tie.staff-position = #3.0
  d ~
  d
}
