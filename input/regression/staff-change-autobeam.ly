\header {
  texidoc = "Staves stay alive long enough to complete an automatic beam."
}

\version "2.16.0"

<<
  {
    g'2 g'8 g'
    \change Staff = "down"
    b' b'
  }
  \context Staff = "down" s1
>>
