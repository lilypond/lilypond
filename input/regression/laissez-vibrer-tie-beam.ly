\header {

  texidoc = "@code{\\laissezVibrer} ties on beamed notes don't trigger
premature beam slope calculation. "

}

\version "2.16.0"

\paper{
  ragged-right=##t
}

{
  c'8 e' g' c''\laissezVibrer r2 |
  c'8 e' g' c'' r2
}
