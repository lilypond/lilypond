\version "2.23.3"

\header {
  texidoc = "Measure-centered bar numbers may be placed beneath
the staves."
}

\layout {
  \context {
    \Score
    centerBarNumbers = ##t
    \override CenteredBarNumberLineSpanner.direction = #DOWN
  }
}

\new StaffGroup <<
  \repeat unfold 10 { c''1 }
  \repeat unfold 10 { c''1 }
>>