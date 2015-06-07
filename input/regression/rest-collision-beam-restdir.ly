\header {
  texidoc = "Beam/rest collision takes offset due to @code{Rest #'direction} into
account properly."
}

\version "2.19.21"
\paper {
  ragged-right = ##t
}


\relative {
  \override Rest.direction = #UP
  \stemDown b''8[ r b]
  \override Rest.direction = #DOWN
  \stemDown b8[ r b]
}

