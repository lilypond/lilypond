\header
{
    texidoc = "  The handling of stems for harmonic notes must be
   completely identical to normal note heads.

  Harmonic heads do not get accidentals or dots."
}

\version "2.1.22"

\score {
 \notes {
    < c'' f''\harmonic >4
    \stemUp
    < c'' f''\harmonic >4.
    < cis'' fis''\harmonic >8
  }

  \paper {
      raggedright = ##t 
  }
}
