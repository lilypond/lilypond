\version "2.11.61"
\header {
  lsrtags = "pitches,staff-notation"
  texidoc = "Quotations take into account the transposition of both
source and target.  In this example, all instruments play sounding
middle C; the target is an instrument in F.  The target part may be
transposed using @code{\\transpose}.  In this case, all the pitches
(including the quoted ones) are transposed.
"
  doctitle = "Quoting another voice with transposition"
}

\addQuote clarinet {
  \transposition bes
  \repeat unfold 8 { d'16 d' d'8 }
}

\addQuote sax {
  \transposition es'
  \repeat unfold 16 { a8 }
}

quoteTest = {
  % french horn
  \transposition f
  g'4
  << \quoteDuring #"clarinet" { \skip 4 } s4^"clar." >>
  << \quoteDuring #"sax" { \skip 4 } s4^"sax." >>
  g'4
}

{
  \set Staff.instrumentName =
    \markup \center-column { Horn \line { in F } }
  \quoteTest
  \transpose c' d' << \quoteTest s4_"up a tone" >>
}
