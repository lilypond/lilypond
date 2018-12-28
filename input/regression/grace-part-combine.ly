
\version "2.21.0"
\header {
  texidoc = "Grace notes may be put in a @code{partCombine}r."
}

\layout { ragged-right= ##t }


\new Staff
\partCombine
\relative {
  c''4 d e f  \grace f16 g1
}
\relative {
  c'4 d e2  g1
}




