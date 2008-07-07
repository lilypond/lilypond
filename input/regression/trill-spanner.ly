
\header {
  texidoc = "The trill symbol and the wavy line are neatly aligned:
the wavy line should appear to come from the crook of the r"

}

\version "2.11.51"
\paper { ragged-right = ##t }
\relative c'' {
  c1\startTrillSpan
  c\stopTrillSpan
}

