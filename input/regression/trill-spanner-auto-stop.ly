\version "2.19.21"
\header {
  texidoc = "Consecutive trill spans work without explicit
@code{\\stopTrillSpan} commands, since successive trill spanners
will automatically become the right bound of the previous trill.
"
}

\paper { ragged-right = ##f }

\relative {
  c''1\startTrillSpan
  c1\startTrillSpan
  c2\stopTrillSpan r
}
