\version "2.17.11"

\header {

  texidoc = "
Beams in a completed tuplet should be continuous.
"
}

{
  \tuplet 3/2 {b16 b b} b8 b8 \tuplet 3/2 {b16 b b}
  \tuplet 3/1 {b16 b b} b8. b \tuplet 3/1 {b16 b b}
}

