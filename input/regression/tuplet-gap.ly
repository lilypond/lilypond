\header {

  texidoc = "The size of the tuplet bracket gap is adjusted to the
    width of the text."

}

\version "2.17.11"

\layout {
  indent = 0.0\mm
  ragged-right = ##t
}


\relative c'' {
  \override TupletNumber.text = #tuplet-number::calc-fraction-text
  \tuplet  12/17  { c8 c4 c8 c8}
}

