\version "2.19.21"

\header {
  texidoc = "Text spanners ending on, or broken across, full-measure
rests extend to the rests, or over the rests, as appropriate."
}

\layout { ragged-right = ##t }

\relative {
  a'1\startTextSpan b1 R1 \break
  \tempo "tempo" R1 a1 b1 R1\stopTextSpan
}
