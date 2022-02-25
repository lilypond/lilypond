\version "2.19.21"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "cannot fit the first page turn onto a single page.  Consider setting first-page-number to an even number."))

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
    \relative {
      a b c d a b c d \break
      c d e f c d e f \break
      d e f g d e f g
    }
  }
}


