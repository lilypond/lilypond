\header {
    texidoc = "Trill spanner"
}

\version "2.7.39"
\layout {
    ragged-right = ##T
}
\relative \new Voice {
    << { c1 \startTrillSpan }
       { s2. \grace { d16[\stopTrillSpan e] } } >>
    c4 }
