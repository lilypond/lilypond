
\version "2.19.21"
\header {
  texidoc = "Dots move to the right when a collision with the (up)flag happens."
}
\layout { ragged-right = ##t}


\relative
{
  \time 8/4
  \set Staff.autoBeaming = ##f
  a16.
  g''16.
  a,4. a8 a8. a16. c,16. g16.
}



