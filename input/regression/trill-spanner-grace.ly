\header {
    texidoc = "Trill spanner can end on a grace note"
}

\version "2.19.21"
\layout {
    ragged-right = ##T
}
\relative \new Voice {
    << { c'1 \startTrillSpan }
       { s2. \grace { d16\stopTrillSpan e } } >>
    c4 }
