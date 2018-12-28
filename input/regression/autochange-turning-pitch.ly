
\version "2.21.0"

\header {
  texidoc=" Other turning points for the autoChanger are possible."
}

\layout { ragged-right= ##t }

\context PianoStaff <<
  \context Staff = "up" {
    \autoChange  \relative { <>^"default c' " b4 c d e }
    \autoChange d' \relative { <>^"switch after d' " b c d e }
  }
  \context Staff = "down" {
    \clef bass
    s1*2
  }
>>
