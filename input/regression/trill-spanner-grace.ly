\header {
    texidoc = "Trill spanner can end on a grace note"
}

\version "2.11.51"
\layout {
    ragged-right = ##T
}
\relative \new Voice {
    << { c1 \startTrillSpan }
       { s2. \grace { d16[\stopTrillSpan e] } } >>
    c4 }
