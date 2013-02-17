\header {

  texidoc = "Nested tuplets do collision resolution, also when they
  span beams."
  
}

\version "2.17.11"
\paper{
  ragged-right=##t
}

{
  \tuplet 7/4 {
    \tuplet 5/4 { c'8 d' e' f' g' } a' b' c''
  }
}
