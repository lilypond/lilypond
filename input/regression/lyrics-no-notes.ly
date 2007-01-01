\header {
  texidoc ="lyric syllables without note attachment are
not centered; Centering may cause unintended effects when
the papercolumn is very wide."
  }
 
\layout{
  ragged-right = ##t
}

\version "2.10.7"
<<
  \new Staff \relative c' {
    \key aes \major
    \context Voice = "1" { 
      f8 f
      \emptyText
      
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
