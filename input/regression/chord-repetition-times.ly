\version "2.19.21"


\header {
  texidoc = "
Chord repetitions are expanded late in the processing order and get
their note events only then.  Check that @code{\\times} still works
correctly on them.
"
}

\relative {
  <c' e g>4 r <c e g>2 ~ |
  \tuplet 3/2 { <c e g>4 q q } \tuplet 3/2 { q q q } |
}
