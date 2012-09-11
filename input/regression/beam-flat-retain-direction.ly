\header
{
  texidoc = "Even very flat but slanted patterns should give slanted beams. "
}

\version "2.16.0"
\layout{
  line-width = 15\cm
  debug-beam-scoring = ##t
}

\relative c'''{
  \time 2/4
  \assertBeamQuant #'(0  . 1) #'(0 . 0) 
  fis16[ dis b ais] cis4
}
