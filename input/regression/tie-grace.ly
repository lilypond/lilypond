
\version "2.8.0"
\header {
  texidoc = "Tieing a grace to the to a following grace or main note works."
}

\layout { ragged-right= ##t }

\context Voice \relative c'' {
  c4 \grace { c8 ~ c16 ~ } c4 
}


