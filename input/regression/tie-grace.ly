
\version "2.1.36"
\header {
    texidoc = "Tieing a grace to the to a following grace or main note works."
}

    \paper { raggedright= ##t }

\score { \notes \context Voice \relative c'' {
    c4 \grace { c8 ~ c16 ~ } c4 
  }
}

