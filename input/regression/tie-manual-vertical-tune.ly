\header {


  texidoc = "If using integers, the tie will vertically tuned for
staff line avoidance. If using a floating point number, this is taken
as the exact location."

}

\version "2.11.11"

\layout {
  ragged-right = ##t
}

\version "2.10.0"
\relative c'' {
  \override Tie #'staff-position = #3
  d4 ~
  \override Tie #'staff-position = #3.0
  d ~
  d
 }
