\version "2.16.0"

\header {
  texidoc = "Each of the staves here should have four tremolo beams."
}

\paper { ragged-right = ##t }
<<
  \repeat tremolo 8 { c64 e64 }
  \repeat tremolo 12 { c64 e64 }
  \repeat tremolo 14 { c64 e64 }
  \repeat tremolo 15 { c64 e64 }
>>
