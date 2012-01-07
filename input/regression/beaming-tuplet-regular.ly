\version "2.15.23"

\header {

  texidoc = "
Beams in a completed tuplet should be continuous.
"
}

{
  \times 2/3 {b16 b b} b8 b8 \times 2/3 {b16 b b}
  \times 1/3 {b16 b b} b8. b \times 1/3 {b16 b b}
}

