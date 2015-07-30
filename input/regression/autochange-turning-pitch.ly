
\version "2.19.25"

\header {
  texidoc=" Other turning points for the autochanger are possible."
}

\layout { ragged-right= ##t }

\context PianoStaff <<
  \context Staff = "up" {
    \autochange  \relative { <>^"default c' " b4 c d e }
    \autochange d' \relative { <>^"switch after d' " b c d e }
  }
  \context Staff = "down" {
    \clef bass
    s1*2
  }
>>
