\header {
    texidoc = "Trill spanner"
}

\version "2.4.0"
\layout {
    raggedright = ##T
}
\relative \new Voice {
    << { c1 \startTrillSpan }
       { s2. \grace { d16[\stopTrillSpan e] } } >>
    c4 }
