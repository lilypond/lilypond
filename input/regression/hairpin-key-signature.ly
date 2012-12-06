\version "2.16.0"

\header {
  texidoc = "Broken hairpins are not printed too high after key signatures.
"
}

\relative c'' {
  \key e \major
  c4^\< c c c \break c c c c\! |
}
