\version "2.15.29"
\header {
  texidoc = "Cross staff glissandi reach their endpoints correctly.
"
}

\new PianoStaff <<
\new Staff = "right" {
  e'''2\glissando
  \change Staff = "left"

  a,,\glissando
  \change Staff = "right"
  b''8
}
\new Staff = "left" {
  \clef bass
  s1 s8
}
>>
