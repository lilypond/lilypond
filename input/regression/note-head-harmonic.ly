\header
{
    texidoc = "  The handling of stems for harmonic notes must be
   completely identical to normal note heads."
}

\version "2.1.9"

\score {
 \notes {
    < c''     f''\harmonic >4
    \stemUp
    < c''     f''\harmonic >
  }

  \paper {
      raggedright = ##t 
  }
}
