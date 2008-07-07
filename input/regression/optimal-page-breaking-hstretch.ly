\version "2.11.51"

\header{
    texidoc="The optimal page breaker will stretch the
systems horizontally so that the vertical spacing will be
more acceptable. The page-spacing-weight parameter
controls the relative importance of vertical/horizontal
spacing. Because ragged-last-bottom is on, only the
first page should be horizontally stretched.
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


