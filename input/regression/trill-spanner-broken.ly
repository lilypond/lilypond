\version "2.16.0"

\header {
  texidoc = "
A TrillSpanner crossing a line break should restart exactly above
the first note on the new line. "
}

{ c'1\startTrillSpan \break
  c' c'\stopTrillSpan
}

\paper {
  ragged-right = ##t
}
