\version "2.21.0"

\header {
  texidoc = "In this test, the vertical distance between two adjacent staves
should be large enough to avoid a clash if the stems are very close."
}

\paper {
  indent = 10\cm
}

<<
  \new Staff << d' \\ <c' a> >>
  \new Staff <a' b'>
>>
