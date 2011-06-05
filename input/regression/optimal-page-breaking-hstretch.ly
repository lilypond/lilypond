\version "2.14.0"

#(set-default-paper-size "a6")

\book {

\header{
    texidoc="The optimal page breaker will make trade-offs between
horizontal and vertical stretching so that the overall spacing
will be more acceptable.  The page-spacing-weight parameter
controls the relative importance of vertical/horizontal spacing.
Because ragged-last-bottom is on, there is no penalty for odd
vertical spacing on the final page.  As a result, only the first
page should be horizontally stretched.
"
}

\paper {
  #(define page-breaking ly:optimal-breaking)
  page-spacing-weight = #10
  ragged-last-bottom = ##t
}

\relative c' {
  \repeat unfold 5 {a b c d} \pageBreak
  \repeat unfold 5 {a b c d}
}

}
