\header {
  texidoc = "SpanBars participate in the horizontal collision system;
the accidentals should not collide with the bar lines."
}

\version "2.11.51"

upper = \relative c' {
  \key f \minor \time 12/8 
  r4. r8 r r r r r r r r 
  e8 e e e e e e e e e e e 
  e, e'! e! e! e! e! e! e! e! e! e! e!
}

lower = { R1. R1. R1. }

\new PianoStaff << \new Staff \upper \new Staff \lower >>

