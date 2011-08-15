\header {
  texidoc = "Staves stay alive long enough to complete an automatic beam."
}

\version "2.15.9"

<<
  {
    g'2 g'8 g'
    \change Staff = "down"
    b' b'
  }
  \context Staff = "down" s1
>>
