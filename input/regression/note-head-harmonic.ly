\header
{
  texidoc = "  The handling of stems for harmonic notes must be
   completely identical to normal note heads.

  Harmonic heads do not get dots. If @code{harmonicAccidentals} is
  unset, they also don't get accidentals."

  
}

\layout {
  ragged-right = ##t 
}

\version "2.16.0"


{
  < c'' f''\harmonic >4
  \stemUp
  < c'' f''\harmonic >4.
  < cis'' fis''\harmonic >8
  \set Staff.harmonicAccidentals = ##f
  < dis'' gis''\harmonic >8
}



