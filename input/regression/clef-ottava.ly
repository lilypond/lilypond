
\header
{
  texidoc = "Ottava brackets and clefs both modify Staff.middleCPosition,
but they don't confuse one another."
}

\version "2.19.21"

\layout { ragged-right = ##t} 

\relative  {
  \clef "alto"
  a' b c a
  \ottava #1
  a b c a
  \clef "bass"
  a b c a
  \ottava #2
  a b c a
  \clef "treble"
  \ottava #-1
  a b c a
}


