\version "2.3.4"
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
    \new Staff \relative c'{
      \repeat volta 2 { c4 d e f g a b c } \alternative{{g2 e }{c1}}
    }
  >>
  \paper {
    \context {
      \Score
      \consists "Volta_engraver"
      \override VoltaBracket #'minimum-space = #0
    }
    \context {
      \Staff
      \remove "Volta_engraver"
    }
  }
}