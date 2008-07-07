\header {

  texidoc = "Nested tuplets do collision resolution, also when they
  span beams."
  
}

\version "2.11.51"
\paper{
  ragged-right=##t
}

{
  \times 4/7 {
    \times 4/5 { c'8 d' e' f' g' } a' b' c''
  }
}
