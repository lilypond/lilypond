
\header {
  texidoc = "The trill symbol and the wavy line are neatly aligned:
the wavy line should appear to come from the crook of the r"

}

\version "2.19.21"
\paper { ragged-right = ##t }
\relative {
  c''1\startTrillSpan
  c\stopTrillSpan
}

