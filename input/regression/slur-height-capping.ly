\version "2.16.0"

\header {
  texidoc = "Slur shaping is not adapted to accommodate objects
towards the edges of slur.  Said objects are thus ignored,
which should make the slur in this regtest flat.  Objects towards
the edges are not, however, ignored in the slur scoring.
"
}

\relative c {
  \clef bass
  c8( d' a d c, d' a d)
  c,8( des' as des c, des' as des) |
}
