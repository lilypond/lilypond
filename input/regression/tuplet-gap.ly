\header {

  texidoc = "The size of the tuplet bracket gap is adjusted to the
    width of the text."

}

\version "2.7.39"

\layout {
  indent = 0.0\mm
  ragged-right = ##t
}


\relative c'' {
  \set tupletNumberFormatFunction = #fraction-tuplet-formatter
  \times  17/12  { c8 c4 c8 c8}
}

