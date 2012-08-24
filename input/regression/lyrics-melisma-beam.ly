\version "2.16.0"
\header
{
  texidoc = "Melismata are triggered by manual beams.  Notes in a
  melisma take their natural spacing over a long syllable."
}

  \layout { ragged-right = ##t }




<<
  \new Staff
  \relative c'' {
    \set Staff.autoBeaming = ##f
    g4 d8[ b8 d8 g8]  g4
  }
  \lyricsto "" \new Lyrics \lyricmode { bla blaa -- bla }
>>


