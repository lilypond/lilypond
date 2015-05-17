\header {
  texidoc = "Because @code{BarLine} grobs take their
extra-positioning-height from their neighbors via the
@code{pure-from-neighbor-interface}, the left edge of an
accidental should never fall to the left of the right
edge of a bar line.  This spacing should also take place when
@code{SpanBar} grobs are present.
"
}

\version "2.19.21"

upper = \relative {
  \key f \minor \time 12/8 
  r4. r8 r r r r r r r r 
  e'8 e e e e e e e e e e e 
  e, e'! e! e! e! e! e! e! e! e! e! e!
}

lower = { R1. R1. R1. }

\new PianoStaff << \new Staff \upper \new Staff \lower >>

