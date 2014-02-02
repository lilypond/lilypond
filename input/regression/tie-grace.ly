
\version "2.19.2"
\header {
  texidoc = "Tying a grace to a following grace or main note works."
}

\layout { ragged-right= ##t }

\context Voice \relative c'' {
  c4 \grace { c8 ~ 16 ~ } c4 
}


