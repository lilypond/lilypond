\version "2.17.6"
\header
{
  texidoc = "The standard piano pedals style comes with Ped symbols.
The pedal string can be also tuned, for example, to a shorter tilde/P variant
at the end of the melody."

}


\context Staff \relative c'{
  c4 d e f g
  \sustainOn b c
  c, d16[  c  c c]  e[ e \sustainOff \sustainOn e e ] f4 \sustainOff 
  g\sustainOn  b \sustainOff c 
  \set Staff.pedalSustainStrings = #'("-" "-P" "P")
  \override Staff.SustainPedal.padding = #-2
  c, \sustainOn d e \sustainOff \sustainOn f
  \sustainOff g b c
}

