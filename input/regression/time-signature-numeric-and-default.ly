\version "2.23.4"

\header {
  texidoc = "@code{\\numericTimeSignature} and @code{\\defaultTimeSignature},
like @code{\\time}, affect all simultaneous staves."
}

<<
  \new Staff {
    e'1
    \numericTimeSignature
    \time 4/4
    e'
    \defaultTimeSignature
    \time 2/2
    e'
  }
  \new Staff {
    c'1 c' c'
  }
>>

