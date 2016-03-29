\header {

  texidoc = "Default beaming patterns can be set for the current time
signature. "
  }

\layout {
  ragged-right = ##t
  }
\version "2.19.40"


\relative {
  \time 5/16
  \set beatStructure = 2,3
  c''8[^"(2+3)" c16 c8]
  \set beatStructure = 3,2
  c8[^"(3+2)" c16 c8]
}
