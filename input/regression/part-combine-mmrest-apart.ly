\version "2.21.0"

\header {
  texidoc = "
The positioning of multimeasure rests in @code{\\partCombineApart}
passages corresponds with @code{\\voiceOne} and @code{\\voiceTwo} even
when using non-standard staves.
"
}

\layout {
  indent = 4\cm
  raggedright = ##t
}

\new StaffGroup
<<
  \new Staff \with
    { \override StaffSymbol.line-count = #4
      instrumentName = \markup \typewriter "\\partCombine"
    }
  {
    \partCombine  { R1^"R1" r1^"r1"  \time 2/4 R2^"R2" r2^"r2" }
    { \partCombineApart R1 r1 \time 2/4 R2 r2 }
    
  }
  \new Staff \with
    { \override StaffSymbol.line-count = #4
      instrumentName = \markup \typewriter "<< ... \\\\ ... >>"
    }
  <<
    { R1 r1 \time 2/4 R2 r2 } \\
    { R1 r1 \time 2/4 R2 r2 }
  >>
>>
