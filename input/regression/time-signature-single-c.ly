\version "2.19.16"

\header {
  texidoc = "The single-C time signature style prints a C for any time signature with 4 in the numerator and prints a cut-C for any time signature with 2 in the numerator."
}

\layout { indent = 0 }

\new Staff {
  \relative f' {
    \override Staff.TimeSignature.style = #'single-C
    \time 1/2 f2^"1/2"
    \time 2/2 f2^"2/2" f
    \time 2/1 f1^"2/1" f
    \time 3/4 f4^"3/4" f f
    \time 4/4 f4^"4/4" f f f
    \time 4/2 f2^"4/2" f f f
    \time 4/1 f1^"4/1" f f f
  }
}
