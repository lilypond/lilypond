\version "2.16.0"
\header {
  texidoc = "
A metronome marking can be added to a multimeasure rest whose
engraver was moved to the Staff, without segfaulting.
"
}


\score {
  \new Staff {
    \tempo 4=150
    R1 |
  }
  \layout {
    \context {
      \Score
      \remove "Metronome_mark_engraver"
      \remove "Staff_collecting_engraver"
    }
    \context {
      \Staff
      \consists "Metronome_mark_engraver"
    }
  }
}

