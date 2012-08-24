\version "2.16.0"


\header {
  texidoc = "
Chord repetitions are expanded late in the processing order and get
their note events only then.  Check that @code{\\times} still works
correctly on them.
"
}

\relative c' {
  <c e g>4 r <c e g>2 ~ |
  \times 2/3 { <c e g>4 q q } \times 2/3 { q q q } |
}
