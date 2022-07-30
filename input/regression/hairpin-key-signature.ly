\version "2.23.12"

\header {
  texidoc = "Broken hairpins are not printed too high after key signatures.
"
}

\relative {
  \key a \major
  c''4^\< c c c \break c c c c\! |
}
