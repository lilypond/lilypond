\header
{
  texidoc = "Broken beams have sane endings even if grobs
  are not present at the broken end. "
  
}

\version "2.12.0"

\new Staff \with {
   \remove Bar_engraver
   \override Beam #'breakable = ##t
} {
   \time 1/8
   c'32 [
   c'32
   c'32
   c'32
   \break
   c'32
   c'32
   c'32
   c'32 ]
}



