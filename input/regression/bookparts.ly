\version "2.11.64"

\header {
  texidoc = "A book can be split into several parts with different paper settings,
using @code{\\bookpart}.

Fonts are loaded into the top-level paper.
Page labels are also collected into the top-level paper."
}

#(set-default-paper-size "a6")

#(define-markup-command (roman-page-number layout props) ()
  (let ((page-number (chain-assoc-get 'page:page-number props)))
    (interpret-markup layout props (format #f "~@r" page-number))))

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
      evenHeaderMarkup = \markup \fill-line { \roman-page-number "SECOND PART" \null }
      oddHeaderMarkup = \markup \fill-line { \null "SECOND PART" \roman-page-number }
    }
    \tocItem \markup "Second part"
    \markup \justify { Second part, with different margins and page header. }
    { c' }
  }

  \tocItem \markup "Third part"
  \markup { Third part }
  \markuplines \table-of-contents
}
