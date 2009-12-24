\version "2.12.0"

\header {
    texidoc="If there are no good places to have a page turn,
the optimal-breaker will just have to recover gracefully. This
should appear on 3 pages.
"
}

\book {
  \paper {
    #(define page-breaking ly:page-turn-breaking)
    paper-height = #40
    print-page-number = ##t
  }

  \score {
    \relative c' {
      a b c d a b c d \break
      c d e f c d e f \break
      d e f g d e f g
    }
  }
}


