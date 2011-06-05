\header {

  texidoc = "Default beaming patterns can be set for the current time
signature. "
  }

\layout {
  ragged-right = ##t
  }
\version "2.14.0"


\relative c'' {
  \time 5/16
  \set beatStructure = #'(2 3)
  c8[^"(2+3)" c16 c8]
  \set beatStructure = #'(3 2)
  c8[^"(3+2)" c16 c8]
}
