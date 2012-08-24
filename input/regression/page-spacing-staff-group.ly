\version "2.16.0"

#(set-default-paper-size "a6")

\book {

  \header {
    texidoc = "By default, the staves within a StaffGroup are spaced more
closely than staves not in a StaffGroup."
  }

  \paper {
    ragged-last-bottom = ##f
  }

  <<
    \new StaffGroup
    <<
      \new Staff c'1
      \new Staff c'1
      \new Staff c'1
    >>
    \new StaffGroup
    <<
      \new Staff c'1
      \new Staff c'1
      \new Staff c'1
    >>
  >>
}
