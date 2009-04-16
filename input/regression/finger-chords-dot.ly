\version "2.13.1"

\header {
  texidoc = "Scripts right of a chord avoid dots."
}

\relative c' {
  \set fingeringOrientations = #'(right)
  <c-\rightHandFinger #1 >4.. <d-3 f>4. r8.
}
