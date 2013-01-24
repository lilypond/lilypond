\version "2.17.11"

\header {
  texidoc = "Tuplet number position is correct when slurs and scripts
are present.
"
}

\relative c'
{
  R1 |
  \break
  \tuplet 3/2 { e8(-> e e) }
}
