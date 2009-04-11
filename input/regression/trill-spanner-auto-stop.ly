\version "2.13.1"
\header {
  texidoc = "Consecutive trill spans work without explicit
@code{\\stopTrillSpan} commands, since successive trill spanners
will automatically become the right bound of the previous trill.
"
}

\paper { ragged-right = ##f }

\relative c'' {
  c1\startTrillSpan
  c1\startTrillSpan
  c2\stopTrillSpan r
}
