\version "2.17.11"

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
    \override TupletBracket.bracket-visibility = ##t
  }
}

voice = {
  \tuplet 3/2 { b8 \change Staff=I c' d' }
  \tuplet 3/2 { d' c' \change Staff=II b }
  \tuplet 3/2 { \change Staff=III c,
               \change Staff=II b
               \change Staff=I c' }
  \tuplet 3/2 { c'[ \change Staff=II b] c }
}

\score {
  \new PianoStaff
  <<
    \new Staff = "I" { s1 }
    \new Staff = "II" { \clef bass \voice }
    \new Staff = "III" { \clef bass s1 }
  >>
}
