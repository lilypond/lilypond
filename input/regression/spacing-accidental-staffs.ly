\version "2.12.0"

\header { 
  texidoc = "Accidentals in different staves do not affect the
spacing of the eighth notes here."
}

\layout { ragged-right = ##t}

\relative c'' <<
  \new Staff {
    \time 4/4

    c8[ c8 cis8 cis8]
    cis8[ cis8 cis8 cis]
  }
  {
    \key d \major cis4 cis4 cis4 cis!4
  }
>>

 




