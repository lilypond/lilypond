
\version "2.6.0"
\header {
  texidoc = "Dots move to the right when a collision with the (up)flag happens."
}

\relative c''
{
  \time 8/4
  \set Staff.autoBeaming = ##f
  a,16.
  g''16.
  a,4. a8 a8. a16. c,16. g16.
}
\layout { raggedright = ##t}


