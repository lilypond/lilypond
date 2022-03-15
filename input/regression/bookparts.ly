\version "2.23.7"

\header {
  texidoc = "A book can be split into several parts with different paper settings,
using @code{\\bookpart}.

Fonts are loaded into the top-level paper.
Page labels are also collected into the top-level paper."
}

#(set-default-paper-size "a6")

\book {
  \tocItem \markup "First part"
  \header { title = "Book with several parts" }
  \markup { First part }
  \markup { with default paper settings. }

  \bookpart {
    \paper {
      left-margin = 20\mm
      right-margin = 20\mm
      line-width = 65\mm
      page-number-type = #'roman-ij-lower
      evenHeaderMarkup = \markup \fill-line {
        \fromproperty #'page:page-number-string
        "SECOND PART"
        \null
      }
      oddHeaderMarkup = \markup \fill-line {
        \null
        "SECOND PART"
        \fromproperty #'page:page-number-string
      }
    }
    \tocItem \markup "Second part"
    \markup \justify { Second part, with different margins and page header. }
    { c' }
  }

  \tocItem \markup "Third part"
  \markup { Third part }
  \markuplist \table-of-contents
}
