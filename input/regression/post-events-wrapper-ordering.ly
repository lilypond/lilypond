\version "2.23.1"

\header {
  texidoc = "When multiple post events are wrapped, they are ordered the same
  as if they had not been wrapped.  Tweaks applied to the wrapper are applied
  to every element."
}

firstSecondThird = -"1st"-"2nd"-"3rd"
bigred = -\tweak font-size 4 -\tweak color #red \etc

{
  s1 r1 \bigred -"1st" \bigred -"2nd" \bigred -"3rd"
  s1 r1 \bigred \firstSecondThird
}
