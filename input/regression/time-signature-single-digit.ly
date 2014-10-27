\version "2.19.16"

\header {
  texidoc = "The single-digit time signature style prints the numerator only."
}

\new Staff {
  \relative d' {
    \override Staff.TimeSignature.style = #'single-digit
    \time 1/2 d2
    \time 2/4 d2
    \time 3/4 d2.
    \time 8/1024 d128
    \time 99999/1 d1
  }
}
