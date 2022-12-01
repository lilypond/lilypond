\version "2.24.1"

\header {
  texidoc = "@code{\\articulate} correctly handles repeats
with alternatives.  They are played in the same order as their
visual rendition suggests."
}

\include "articulate.ly"

\score {
  \articulate {
    \repeat volta 2 {
      bes1
      \alternative {
        \volta 1 b!
        \volta 2 bis
      }
    }
  }
  \midi { }
  \layout { }
}
