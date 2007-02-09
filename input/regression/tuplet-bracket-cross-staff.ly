\version "2.11.16"
\paper { ragged-right = ##t }
\layout {
  \context {
    \Score
    \override TupletBracket #'bracket-visibility = ##t
  }
}

voice = {
  \times 2/3 { b8 \change Staff=RH c' d' }
  \times 2/3 { d' c' \change Staff=LH b }
}

\score {
  \new PianoStaff
  <<
    \new Staff = "RH" { s4 s4 s4 }
    \new Staff = "LH" { \clef bass \voice }
  >>
}
