\version "2.16.0"

#(set-default-paper-size "a6")

\book {

\header{
    texidoc="The optimal page breaker will make trade-offs between
horizontal and vertical stretching so that the overall spacing
will be more acceptable.  The @code{page-spacing-weight} parameter
controls the relative importance of vertical/@/horizontal spacing.
Because @code{ragged-last-bottom} is on, there is no penalty for odd
vertical spacing on the final page.  As a result, only the first
page should be horizontally stretched.
"
}

\paper {
  #(define page-breaking ly:optimal-breaking)
  page-spacing-weight = #100 % default is 10
  ragged-last-bottom = ##t
}

\relative c' {
  <>_"this page stretched horizontally"
  \repeat unfold 5 {a b c d} \pageBreak
  <>_"this page with natural spacing"
  \repeat unfold 5 {a b c d}
}

}
