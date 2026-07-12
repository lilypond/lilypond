\version "2.23.9"

\header {
  texidoc = "@code{\\articulate} correctly handles trill spans ending on
multi-measure rests.  It does not error for skip events."
}

\include "articulate.ly"
\score {
\unfoldRepeats \articulate {
   b1 \startTrillSpan |
   R1 \stopTrillSpan |
   b2 r |
   s1\startTrillSpan |
   s1\stopTrillSpan |
   b1 |
}
\midi {}
}

