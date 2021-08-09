\version "2.19.21"

\header{
    texidoc="The page-turn breaker will put a page turn after
a rest unless there is a 'special' bar line within the rest,
in which case the turn will go after the special bar line.
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
    paper-height = #35
    auto-first-page-number = ##t
    print-page-number = ##t
    print-first-page-number = ##t
    tagline = ##f
  }

  \score {
    \relative {
      a b c d a b c d \break
      c d e f c d e f R1*4
      \repeat unfold 13 {d4 e f g} \break
      c d e f c d e f R1*2 \bar "||" R1*2
      \repeat unfold 15 {d4 e f g}
    }
  }
}

