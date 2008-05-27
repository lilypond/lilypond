\version "2.11.36"
\layout { ragged-right= ##t }
\header {
  lsrtags = "repeats,staff-notation,chords"
  texidoc = "By adding the @code{Volta_engraver} to the relevant
staff, volte can be put under chords."
  doctitle = "Volta under chords"
}

% chords above volta
\score {
  <<
    \chords {
      c1
       c
  }
  \new Staff \with { \consists Volta_engraver } {
     \repeat volta 2 { c'1 }
     \alternative { c' }
     }
  >>
  \layout {
    \context {
      \Score
      \remove Volta_engraver
    }
  }
}

