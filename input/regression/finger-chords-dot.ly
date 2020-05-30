\version "2.19.21"

\header {
  texidoc = "Scripts right of a chord avoid dots."
}

\relative {
  \set fingeringOrientations = #'(right)
  <c'\rightHandFinger #1 >4.. <d-3 f>4. r8.
  <d a'-2 b d e>4. r8  <d-1 a'-2 b-3 d-4 e-5>4. r8
}
