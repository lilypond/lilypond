\header {
  texidoc = "Broken tie lengths are not affected by clefs
in other staves."
}

\version "2.16.0"

\layout {
  ragged-right = ##t
}

<<
  \new Staff \relative c'''{ e1 ~ \break e }
  \new Staff \relative c{ \clef bass a \clef treble cis }
>>
