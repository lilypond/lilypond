\version "2.11.16"

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
}

\score {
  \new PianoStaff
  <<
    \new Staff = "I" { s4 s4 s4 }
    \new Staff = "II" { \clef bass \voice }
    \new Staff = "III" { \clef bass s4 s4 s4 }
  >>
}
