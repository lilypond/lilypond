
\version "2.19.21"
\header {
  texidoc = "Grace notes may be put in a @code{partcombine}r."
}

\layout { ragged-right= ##t }


\new Staff
\partcombine 
\relative {
  c''4 d e f  \grace f16 g1
}
\relative {
  c'4 d e2  g1
}




