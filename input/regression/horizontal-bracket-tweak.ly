\version "2.15.5"

\header {
  texidoc = "Horizontal brackets are created with the correct event-cause, ensuring
tweaks are applied to the correct spanner."
}

\layout {
  \context {
    \Voice
    \consists "Horizontal_bracket_engraver"
  }
}

\relative c' {
  c1-\tweak #'thickness #6 \startGroup
  c1\startGroup
  c1\stopGroup
  c1\stopGroup
}
