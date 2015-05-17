\version "2.19.21"

\header {
  texidoc = "Slurs and ties should never share extremal control points.
"
}
\paper { ragged-right = ##f }

\relative {
  c''1~( c2 g)
}
