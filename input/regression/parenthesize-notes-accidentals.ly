\version "2.19.19"

\header {
  texidoc = "Parentheses around notes also include accidentals and dots;
they are centered on the vertical center of the combined enclosed items."
}

\score {
  \new Staff {
    \parenthesize ais'4. \parenthesize des''4
  }
}

