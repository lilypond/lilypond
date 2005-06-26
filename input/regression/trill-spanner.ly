\header {
    texidoc = "Trill spanner"
}

\version "2.6.0"
\layout {
    raggedright = ##T
}
\relative \new Voice {
    << { c1 \startTrillSpan }
       { s2. \grace { d16[\stopTrillSpan e] } } >>
    c4 }
