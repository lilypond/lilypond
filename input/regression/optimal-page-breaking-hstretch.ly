\version "2.9.13"

\header{
    texidoc="The optimal page breaker will stretch the
systems horizontally so that the vertical spacing will be
more acceptable. The page-spacing-weight parameter
controls the relative importance of vertical/horizontal
spacing.
"
}

\paper {
  #(define page-breaking ly:optimal-breaking)
  page-spacing-weight = #10
  ragged-last-bottom = ##f
}

\relative c' {
  \repeat unfold 5 {a b c d}
}


