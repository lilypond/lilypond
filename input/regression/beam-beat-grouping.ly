\header {

  texidoc = "Beaming patterns obey the @code{beatGrouping} property. "
  }

\layout {
  ragged-right = ##t
  }
\version "2.12.0"


\relative c'' {
  \time 5/16
  \set beatGrouping = #'(2 3)
  c8[^"(2+3)" c16 c8]
  \set beatGrouping = #'(3 2)
  c8[^"(3+2)" c16 c8]
}
