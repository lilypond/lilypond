\version "2.2.0"
\header {

    texidoc = "@cindex ChordNames repeats 
To make the chord names appear below the brackets of the alternative
endings of a repeat, move the Volta_engraver to the Score level.
" }

\score{
  <<
    \new ChordNames \chords{
      \repeat volta 2 {
        f1:maj f:7} \alternative{{bes:7}{c:maj}}
    }
    \new Staff \notes \relative c'{
      \repeat volta 2 { c4 d e f g a b c } \alternative{{g2 e }{c1}}
    }
  >>
  \paper {
    \context {
      \ScoreContext
      \consists "Volta_engraver"
      \override VoltaBracket #'minimum-space = #0
    }
    \context {
      \StaffContext
      \remove "Volta_engraver"
    }
  }
}