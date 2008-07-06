
\header
{
  texidoc = "Ottava brackets and clefs both modify Staff.middleCPosition,
but they don't confuse one another."
}

\version "2.11.51"

\layout { ragged-right = ##t} 

\relative c''  {
  \clef "alto"
  a b c a
  #(set-octavation 1)
  a b c a
  \clef "bass"
  a b c a
  #(set-octavation 2)
  a b c a
  \clef "treble"
  #(set-octavation -1)
  a b c a
}


