
\version "2.16.0"

\header{
  texidoc="
If @code{Score.skipBars} is set,
the signs for four, two, and one measure rest are combined to
produce the graphical representation of rests for up to 10 bars.
The number of bars will be written above the sign.
"
}

thenotes =  \relative cis' {
  \set Score.skipBars = ##t
  \time 4/4
  R1 |
  R1*1 |
  R1*2 |
  R1*3 |
  R1*4 |
  R1*5 |
  R1*6 |
  R1*7 |
  R1*8 |
  R1*9 |
  R1*10 |
  R1*11 | 
}

<< \context Staff \thenotes

 >>


