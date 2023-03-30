\version "2.24.2"

\header {
  texidoc = "@code{Custos_engraver} accepts (and ignores) unpitched notes."
}

\layout {
  \context {
    \Staff
    \consists "Custos_engraver"
  }
  ragged-right = ##t
}



{
  1
}

{
  1 \break 1
}
