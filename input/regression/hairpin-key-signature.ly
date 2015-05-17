\version "2.19.21"

\header {
  texidoc = "Broken hairpins are not printed too high after key signatures.
"
}

\relative {
  \key e \major
  c''4^\< c c c \break c c c c\! |
}
