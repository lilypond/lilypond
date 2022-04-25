\version "2.23.9"

\header {
  texidoc = "@code{\\articulate} correctly handles trill spans ending on multi-measure rests."
}

\include "articulate.ly"
\score {
\unfoldRepeats \articulate {
   b1 \startTrillSpan |
   R1 \stopTrillSpan |
   b2 r |
   b1
}
\midi {}
}

