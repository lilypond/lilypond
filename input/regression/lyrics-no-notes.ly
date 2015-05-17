\header {
  texidoc ="Lyric syllables without note attachment are
aligned correctly even if the paper column is very wide."
}
 
\layout{ ragged-right = ##t }

\version "2.19.21"
<<
  \override Score.PaperColumn.keep-inside-line = ##f
  \new Staff \relative {
    \key aes \major
    \context Voice = "1" { 
      f'8 f
      \textLengthOff
      
      f^"xxxxxxxxxxxxxxxxxxxxxxxxxx"  f
    }
  }
  \new Lyrics \lyricmode {
    \skip 8
    \skip 8
    xx8
    x8
  }
>>
