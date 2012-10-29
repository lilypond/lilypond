\version "2.17.6"

\header {
  texidoc = "Staves in a PianoStaff remain alive as long as any of
the staves has something interesting."
}

\layout {
  \context {
    \Staff
    \RemoveEmptyStaves
    \override VerticalAxisGroup.remove-first = ##t
  }
}

<<
  \new Staff { c'1 \break c'1 \break c'1 }
  \new PianoStaff
  <<
    \new Staff { d'1 R1 R1 }
    \new Staff { R1 e'1 R1 }
  >>
>>
