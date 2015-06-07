\version "2.19.21"

\header {
  texidoc = "Scripts right of a chord avoid dots."
}

\relative {
  \set fingeringOrientations = #'(right)
  <c'-\rightHandFinger #1 >4.. <d-3 f>4. r8.
}
