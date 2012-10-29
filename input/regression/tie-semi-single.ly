
\header {


  texidoc = "Like normal ties, single semities (LaissezVibrerTie or
RepeatTie) get their direction from the stem direction, and may be
tweaked with @code{'direction}."


}
\version "2.17.6"
\layout{ragged-right=##t}

{
  r4
  c'\laissezVibrer\repeatTie
  \stemUp
  b'\laissezVibrer\repeatTie
  r
  
  \stemDown
  b'\laissezVibrer\repeatTie
  r
  c''\laissezVibrer\repeatTie
  r
  
  \override LaissezVibrerTie.direction = #DOWN
  \override RepeatTie.direction = #DOWN
  c''\laissezVibrer_"override"\repeatTie
} 
