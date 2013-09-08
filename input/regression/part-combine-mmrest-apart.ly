\version "2.17.6"

\header {
  texidoc = "
The positioning of multimeasure rests in @code{\\partcombineApart}
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
      instrumentName = \markup \typewriter "\\partcombine"
    }
  {
    \partcombine  { R1^"R1" r1^"r1"  \time 2/4 R2^"R2" r2^"r2" }
    { \partcombineApart R1 r1 \time 2/4 R2 r2 }
    
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
