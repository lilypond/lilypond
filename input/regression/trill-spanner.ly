\header {
    texidoc = "Trill spanner"
}

\version "2.7.13"
\layout {
    raggedright = ##T
}
\relative \new Voice {
    << { c1 \startTrillSpan }
       { s2. \grace { d16[\stopTrillSpan e] } } >>
    c4 }
