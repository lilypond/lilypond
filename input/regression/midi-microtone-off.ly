\header {

  texidoc = "Microtonal shifts should be corrected before the start of
  the next (possibly grace) note.  "
}

\version "2.16.0"

\score
{

  \relative c' {
    a geseh \acciaccatura a geseh
  }

  \midi {}
}
