\header {
    texidoc = "Tieing a grace to the to a following grace or main note works."
}

\score { \notes \context Voice \relative c'' {
    c4 \grace { c8 ~ c16 ~ } c4 
  }
}
