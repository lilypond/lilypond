\header {
  texidoc ="Lyric syllables without note attachment are not
centered.  Centering may cause unintended effects when the
paper column is very wide."
}
 
\layout{ ragged-right = ##t }

\version "2.14.0"
<<
  \override Score.PaperColumn #'keep-inside-line = ##f
  \new Staff \relative c' {
    \key aes \major
    \context Voice = "1" { 
      f8 f
      \textLengthOff
      
      f^"xxxxxxxxxxxxxxxxxxxxxxxxxx"  f
    }
  }
  \new Lyrics \lyricmode {
    \skip 8
    \skip 8
    x8
    x8
  }
>>
