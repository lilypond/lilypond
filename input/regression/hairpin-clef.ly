\version "2.15.28"

\header {
  texidoc = "Broken hairpins are not printed too high after treble clefs.
"
}

\relative c'' {
  c4^\< c c c \break c c c c\! |
}
