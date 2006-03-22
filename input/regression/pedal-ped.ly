\version "2.8.0"
\header
{
  texidoc = "The standard piano pedals style comes with Ped symbols.
The pedal string can be also tuned, for example, to a shorter tilde/P variant
at the end of the melody."

}


\context Staff \relative c'{
  c4 d e f g
  \sustainDown b c
  c, d16[  c  c c]  e[ e \sustainUp \sustainDown e e ] f4 \sustainUp 
  g\sustainDown  b \sustainUp c 
  \set Staff.pedalSustainStrings = #'("-" "-P" "P")
  \override Staff.SustainPedal   #'padding = #-2
  c, \sustainDown d e \sustainUp \sustainDown f
  \sustainUp g b c
}

