\version "2.17.6"

#(set-default-paper-size "a6")

\book {

  \header {
    texidoc = "The stretchability property affects the amount that staves will
move under extreme stretching, but it does not affect the default distance
between staves."
  }

  <<
    \new Staff { c'1 \pageBreak c'1 }
    \new Staff \with {
      \override VerticalAxisGroup.default-staff-staff-spacing.stretchability = #50
    } { c'1 c'1 }
    \new Staff { c'1 c'1 }
  >>
}
