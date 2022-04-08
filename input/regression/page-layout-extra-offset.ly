\version "2.23.8"

\header {
  texidoc = "By setting @code{extra-offset} within the
@code{line-break-system-details} of @code{NonMusicalPaperColumn},
systems may be moved in relation either to their default position on the
printable area of the page or the absolute position specified by
@code{X-offset} or @code{Y-offset} within
@code{line-break-system-details}."

}

#(set-default-paper-size "a6" 'portrait)

\paper {
  indent = 0.0
  oddHeaderMarkup = "header"
  oddFooterMarkup = "footer"
  system-separator-markup = \slashSeparator
  ragged-right = ##t
}

\book {
  \score {
    {
      \once \override
      Score.NonMusicalPaperColumn.line-break-system-details =
      #'((extra-offset . (8.0 . 0.0)))
      c1 c1 \break

      \once \override
      Score.NonMusicalPaperColumn.line-break-system-details =
      #'((extra-offset . (12.0 . 8.0)))
      c1 c1 \break

      \once \override
      Score.NonMusicalPaperColumn.line-break-system-details =
      #'((X-offset . 8.0)
         (Y-offset . 36.0)
         (extra-offset . (-8.0 . 30.0))
      )
      c1 c1
    }
  }
}
