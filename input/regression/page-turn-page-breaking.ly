\version "2.11.51"

\header{
    texidoc="The page-turn breaker will put a page turn after
a rest unless there is a 'special' barline within the rest,
in which case the turn will go after the special barline.
"
}

\layout {
  \context {
    \Staff
    \consists "Page_turn_engraver"
  }
}

\book {

  \paper {
    #(define page-breaking ly:page-turn-breaking)
    paper-height = #65
    auto-first-page-number = ##t
    print-page-number = ##t
    print-first-page-number = ##t
  }

  \score {
    \relative c' {
      a b c d a b c d \break
      c d e f c d e f R1*4
      \repeat unfold 13 {d4 e f g} \break
      c d e f c d e f R1*2 \bar "||" R1*2
      \repeat unfold 15 {d4 e f g}
    }
  }
}

