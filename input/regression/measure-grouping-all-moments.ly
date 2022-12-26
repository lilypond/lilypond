\version "2.25.1"

\header
{
  texidoc = "The @code{Measure_grouping_engraver} also starts triangles and
brackets at moments where no new note or rest starts."
}

\new Staff \with {
  \consists Measure_grouping_engraver
}
{
  \time 6/8
  c'2.
  4. 4.
  4 4 4
  \time 3,3,2 8/8
  8 8 8 8 8 8 8 8
  4 4 4 4
}
