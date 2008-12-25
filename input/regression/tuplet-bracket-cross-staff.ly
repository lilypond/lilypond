\version "2.12.0"

\paper {
  ragged-right = ##t
}

\header {
  texidoc = "Cross-staff tuplets are drawn correctly,
even across multiple staves."
}

\layout {
  \context {
    \Score
    \override TupletBracket #'bracket-visibility = ##t
  }
}

voice = {
  \times 2/3 { b8 \change Staff=I c' d' }
  \times 2/3 { d' c' \change Staff=II b }
  \times 2/3 { \change Staff=III c,
               \change Staff=II b
               \change Staff=I c' }
  \times 2/3 { c'[ \change Staff=II b] c }
}

\score {
  \new PianoStaff
  <<
    \new Staff = "I" { s1 }
    \new Staff = "II" { \clef bass \voice }
    \new Staff = "III" { \clef bass s1 }
  >>
}
