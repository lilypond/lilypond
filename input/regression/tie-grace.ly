
\version "2.7.13"
\header {
  texidoc = "Tieing a grace to the to a following grace or main note works."
}

\layout { raggedright= ##t }

\context Voice \relative c'' {
  c4 \grace { c8 ~ c16 ~ } c4 
}


