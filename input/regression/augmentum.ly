\header {
  texidoc = "Augmentum dots are accounted for in horizontal spacing."
}

\version "2.11.51"

\include "gregorian.ly"
\score {
  \new VaticanaVoice {
    \[ \augmentum a \flexa \augmentum g \]
    \augmentum g
  }
}
