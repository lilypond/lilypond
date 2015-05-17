\version "2.19.21"

\header {
  texidoc = "Setting @code{'zigzag} style for spanners does not
cause spacing problems: in this example, the first text markup and
zigzag trillspanner have the same outside staff positioning as
the second markup and default trillspanner.
"
}

\relative {
  c'1^"text"
  \override TrillSpanner.style = #'zigzag
  c1\startTrillSpan
  c2^\stopTrillSpan c^"text"
  \revert TrillSpanner.style
  c1\startTrillSpan
  c1^\stopTrillSpan
}
