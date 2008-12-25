\header {
  texidoc = "Ambituses use actual pitch not lexicographic ordering."
  }

\version "2.12.0"

\paper {
  ragged-right=##t
}

\new Voice \with { \consists "Ambitus_engraver" } {
  \clef F c eis fes
}
