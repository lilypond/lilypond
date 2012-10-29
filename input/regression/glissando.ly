\version "2.17.6"

\header{

  texidoc=" Between notes, there may be simple glissando lines.
Here, the first two glissandi are not consecutive.

The engraver does no time-keeping, so it involves some trickery to get
<< @{ s8 s8 s4 @} @{ c4 \\gliss d4 @} >> working correctly.

"
}


\layout{
  line-width = 50.\mm
  indent = 0
}

\new Staff \relative c''{
  \override Glissando.breakable = ##t
  
  %% gliss non gliss and 
  c4 \glissando d e \glissando f \glissando \break
  %% consecutive 
  c \glissando d, \glissando e'
  << { e8 \glissando g8 } \\
     { \repeat unfold 4 d16 } >>
  \override Glissando.style = #'zigzag
  c4 \glissando c,, \glissando c' \glissando d
}


