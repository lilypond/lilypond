\version "2.19.21"

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

\relative {
  c'1-\tweak thickness #6 \startGroup
  c1\startGroup
  c1\stopGroup
  c1\stopGroup
}
