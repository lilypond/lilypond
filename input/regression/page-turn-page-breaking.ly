\version "2.9.13"

\header{
    texidoc="The page-turn breaker will put a page turn after
a rest unless there is a 'special' barline within the rest,
in which case the turn will go after the special barline.
"
}

\paper {
  #(define page-breaking ly:page-turn-breaking)
  paper-height = #70
}

\layout {
  \context {
    \Staff
    \consists "Page_turn_engraver"
  }
}

\relative c' {
  a b c d a b c d \break
  c d e f c d e f R1*4
  \repeat unfold 15 {d4 e f g} \break
  c d e f c d e f R1*2 \bar "||" R1*2
  \repeat unfold 15 {d4 e f g}
}

