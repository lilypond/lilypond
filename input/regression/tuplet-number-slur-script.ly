\version "2.15.35"

\header {
  texidoc = "Tuplet number position is correct when slurs and scripts
are present.
"
}

\relative c'
{
  R1 |
  \break
  \times 2/3 { e8(-> e e) }
}
