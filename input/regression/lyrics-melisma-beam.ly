\version "2.19.21"
\header
{
  texidoc = "Melismata are triggered by manual beams.  Notes in a
  melisma take their natural spacing over a long syllable."
}

  \layout { ragged-right = ##t }




<<
  \new Staff
  \relative {
    \set Staff.autoBeaming = ##f
    g'4 d8[ b8 d8 g8]  g4
  }
  \new Lyrics \lyricsto "" { bla blaa -- bla }
>>


