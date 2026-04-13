\version "2.25.35"

\header {
  categories = "Pitches, Staff notation"

  texidoc = "
Quotations take into account the transposition of both source and
target. In this example, all instruments play sounding middle@tie{}c;
the target is an instrument in@tie{}f. The target part may be
transposed using @code{\\transpose}. In this case, all the pitches
(including the quoted ones) are transposed.
"

  doctitle = "Quoting another voice with transposition"
} % begin verbatim


\addQuote clarinet {
  \transposition bes
  \*8 { d'16 d' d'8 }
}

\addQuote sax {
  \transposition es'
  \*16 a8
}

quoteTest = {
  % french horn
  \transposition f
  g'4
  << \quoteDuring "clarinet" { \skip 4 } s4^"clar." >>
  << \quoteDuring "sax" { \skip 4 } s4^"sax." >>
  g'4
}

{
  \new Staff \with {
    instrumentName = \markup { \column { Horn "in F" } }
  }
  \quoteTest
  \transpose c' d' << \quoteTest s4_"up a tone" >>
}
