\header {
  texidoc = "Augmentum dots are accounted for in horizontal spacing."
}

\include "gregorian-init.ly"
\score {
  \new VaticanaVoice {
    \[ \augmentum a \flexa \augmentum g \]
    \augmentum g
  }
}