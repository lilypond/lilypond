\version "2.19.21"

\header { 
  texidoc = "Accidentals in different staves do not affect the
spacing of the eighth notes here."
}

\layout { ragged-right = ##t}

\relative <<
  \new Staff {
    \time 4/4

    c''8[ c8 cis8 cis8]
    cis8[ cis8 cis8 cis]
  }
  {
    \key d \major cis4 cis4 cis4 cis!4
  }
>>

 




