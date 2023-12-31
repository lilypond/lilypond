\version "2.19.21"

\header {
  texidoc = "The single-number time signature style prints the numerator only."
}

\new Staff {
  \relative {
    \override Staff.TimeSignature.style = #'single-number
    \time 1/2 d'2
    \time 2/4 d4 d
    \time 3/4 d2.
    \time 16/4 d\longa
  }
}
