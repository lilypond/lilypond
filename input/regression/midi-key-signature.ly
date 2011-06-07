\version "2.14.0"
\header {
  texidoc = "MIDI key signatures are output, using an approximate
key signature if MIDI format cannot represent the true key signature"
}

\include "arabic.ly"
\score {
  \relative do' {
    \key fa \bayati
    fa4 solsb lab sib
  }
  \midi { }
  \layout { }
}
