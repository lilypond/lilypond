\version "2.15.2"

\header {
  texidoc = "Text spanners ending on, or broken across, full-measure
rests extend to the rests, or over the rests, as appropriate."
}

\layout { ragged-right = ##t }

\relative c'' {
  a1\startTextSpan b1 R1 \break
  \tempo "tempo" R1 a1 b1 R1\stopTextSpan
}
