\version "2.11.51"
\header {
    texidoc = "Clefs are also folded under cross staff constructs."
}

\layout { ragged-right = ##t}

\context PianoStaff <<
  \new Staff = "up"
  \relative c'' <<
    {
      \stemDown
      f16[ \change Staff = down \stemUp
      \clef treble g,]

    } \\
  >>
  \new Staff = "down" {
    \time 3/8 \clef bass s8
  }
>>

