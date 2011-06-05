\version "2.14.0"
\header {
  lsrtags = "rhythms,expressive-marks"
  texidoc = "This code demonstrates how to change the number of
augmentation dots on a single note."
  doctitle = "Changing the number of augmentation dots per note"
}

\relative c' {
  c4.. a16 r2 |
  \override Dots #'dot-count = #4
  c4.. a16 r2 |
  \override Dots #'dot-count = #0
  c4.. a16 r2 |
  \revert Dots #'dot-count
  c4.. a16 r2 |
}
