
\version "2.19.21"
\header {
  texidoc = "Tying a grace to a following grace or main note works."
}

\layout { ragged-right= ##t }

\context Voice \relative {
  c''4 \grace { c8 ~ 16 ~ } c4 
}


