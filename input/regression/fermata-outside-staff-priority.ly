\version "2.25.1"

\header {
  texidoc = "Trills and trill spanners should be below fermatas.
Fermatas should be below ottava spanners."
}

{
  \ottava #1
  g''2->\fermata\startTrillSpan
  \ottava #0
  r\stopTrillSpan
}

{
  \ottava #1
  g''2->\fermata\trill
  \ottava #0
  r
}




