\version "2.23.11"

\header {
  texidoc = "The @code{hair-thickness} property sets the thickness of
thin bar lines, the @code{thick-thickness} property sets the thickness
of thick bar lines, and the @code{kern} property sets the spacing
within composite bar lines."
}

bars = {
  s1 \bar "!"
  s1 \bar "'"
  s1 \bar ","
  s1 \bar ";"
  s1 \bar "S"
  s1 \bar ".|:"
  s1 \bar ":|."
  s1 \bar "[|:"
  s1 \bar ":|]"
  s1 \bar "k"
}

\new ChoirStaff <<
  \new Staff \with {
    instrumentName = "default"
  } \bars

  \new Staff \with {
    instrumentName = \markup \column { "3x hair-" thickness }
    \override BarLine.hair-thickness = #5.7
  } \bars

  \new Staff \with {
    instrumentName = \markup \column { "2x thick-" thickness }
    \override BarLine.thick-thickness = #12.0
  } \bars

  \new Staff \with {
    instrumentName = "3x kern"
    \override BarLine.kern = #9.0
  } \bars
>>
