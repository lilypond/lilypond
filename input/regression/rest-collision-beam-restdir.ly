\header {
  texidoc = "Beam/rest collision takes offset due to @code{Rest #'direction} into
account properly."
}

\version "2.11.51"
\paper {
  ragged-right = ##t
}


\relative c''' {
  \override Rest #'direction = #UP
  \stemDown b8[ r b]
  \override Rest #'direction = #DOWN
  \stemDown b8[ r b]
}

