\header {


  texidoc = "If using integers, the tie will vertically tuned for
staff line avoidance. If using a floating point number, this is taken
as the exact location."

}

\version "2.11.51"

\layout {
  ragged-right = ##t
}

\version "2.11.51"
\relative c'' {
  \override Tie #'staff-position = #3
  d4 ~
  \override Tie #'staff-position = #3.0
  d ~
  d
 }
