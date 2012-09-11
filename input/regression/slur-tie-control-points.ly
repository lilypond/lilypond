\version "2.16.0"

\header {
  texidoc = "Slurs and ties should never share extremal control points.
"
}
\paper { ragged-right = ##f }

\relative c'' {
  c1~( c2 g)
}
