\version "2.21.2"

\header{
  texidoc = "Lyric syllables of widely varying length do not disproportionately
  affect bar lengths.  In this example both scores should fit on one line.  The
  first score's system should not exceed line-width.  The bars in the second
  score's system should be of roughly equal length.
  "
}

\new Staff <<
  { r4 a a2 a4 a2 }
  \addlyrics {
    \override LyricText.X-offset = #0
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"  a a a
  }
>>

\new Staff <<
  \relative f' { \repeat unfold 8 a4 }
  \addlyrics { \repeat unfold 4 la \repeat unfold 4 straight }
>>
\layout {
  ragged-right = ##f
}



