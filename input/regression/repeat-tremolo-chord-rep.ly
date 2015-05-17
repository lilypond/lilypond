\version "2.19.21"

\header {
  texidoc = "Tremolos work with chord repetitions."
}

\relative {
  <c' e g>1
  \repeat tremolo 4 q16
  \repeat tremolo 4 { q16 }
  \repeat tremolo 4 { c16 q16 }
}
