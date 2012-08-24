
\version "2.16.0"
\header{
  texidoc="
Slurs should look nice and symmetric.  The curvature may increase
only to avoid noteheads, and as little as possible.  Slurs never
run through noteheads or stems.
"
}

\layout{
  line-width = 120.\mm
}

\relative c''{
  \time 3/4
  \slurUp
  \stemNeutral a ( \stemDown a \stemNeutral  a) a( c  a) a( e'  a,) a( g'  a,)
  \stemUp a( e'  a,)
  \break
  \slurDown
  \stemNeutral c ( \stemUp c \stemNeutral  c) c ( a  c) c( d,  c') c( f,  c')
  \stemDown c( f,  c')
}
