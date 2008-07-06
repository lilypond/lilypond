\header {
  texidoc = "Augmentum dots are accounted for in horizontal spacing."
}

\version "2.11.51"

\include "gregorian-init.ly"
\score {
  \new VaticanaVoice {
    \[ \augmentum a \flexa \augmentum g \]
    \augmentum g
  }
}
