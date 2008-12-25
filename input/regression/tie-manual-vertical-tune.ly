\version "2.12.0"

\header {
  texidoc = "If using integers, the tie will vertically tuned for
staff line avoidance. If using a floating point number, this is taken
as the exact location."
}

\relative c'' {
  \override Tie #'staff-position = #3
  d4 ~
  \override Tie #'staff-position = #3.0
  d ~
  d
}
