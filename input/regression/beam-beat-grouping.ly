\header {

  texidoc = "Default beaming patterns can be set for the current time
signature by @code{\\setBeatGrouping}. "
  }

\layout {
  ragged-right = ##t
  }
\version "2.13.4"


\relative c'' {
  \time 5/16
  \setBeatGrouping  #'(2 3)
  c8[^"(2+3)" c16 c8]
  \setBeatGrouping  #'(3 2)
  c8[^"(3+2)" c16 c8]
}
