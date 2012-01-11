\version "2.15.24"

\header {
  texidoc = "Beams do not collide with flags.
"
}

\relative c' <<
  { \voiceOne c'8 r } \\
  { \voiceThree c,8.[ c'16] }
>>
