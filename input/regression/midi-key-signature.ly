\version "2.19.21"
\header {
  texidoc = "MIDI key signatures are output, using an approximate
key signature if MIDI format cannot represent the true key signature"
}

\include "arabic.ly"
\score {
  \relative {
    \key fa \bayati
    fa'4 solsb lab sib
  }
  \midi { }
  \layout { }
}
