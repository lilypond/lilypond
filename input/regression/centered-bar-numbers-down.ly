\version "2.25.35"

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
  \*10 c''1
  \*10 c''1
>>