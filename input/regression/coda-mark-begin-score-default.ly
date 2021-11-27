\version "2.23.6"

\header {
  texidoc = "@code{\\codaMark \\default} at the beginning of the score
does not create a mark.  A single coda mark should appear at the
beginning of the second measure and a double coda mark should appear
at the end."
}

{
  \codaMark \default R1 |
  \codaMark \default R1 |
  \codaMark \default
}
