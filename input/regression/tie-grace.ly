
\version "2.3.4"
\header {
    texidoc = "Tieing a grace to the to a following grace or main note works."
}

    \paper { raggedright= ##t }

\score {  \context Voice \relative c'' {
    c4 \grace { c8 ~ c16 ~ } c4 
  }
}

