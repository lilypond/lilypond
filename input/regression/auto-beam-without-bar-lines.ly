\version "2.25.21"

\header {
  texidoc = "Bar lines are disabled for this test to prove that certain beaming
decisions do not depend on them.  The notes should be beamed in groups of 2, 6,
6, and@tie{}2."
}

#(ly:set-option 'warning-as-error #t)

\new Staff \with {
  \remove Bar_engraver
} \fixed c' {
  \time 3/4
  \partial 8*2
  c8 c
  d8 d d d d d
  \repeat volta 2 {
    e8 e e e
    \alternative {
      \volta 1 { f8 f }
      \volta 2 { g8 g }
    }
  }
}
