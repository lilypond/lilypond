\version "2.23.14"

\header {
  texidoc = "Slurs on dotted notes should have a similar distance to
the note heads as slurs on non-dotted notes if this does not lead
to a collision."
}

\relative c''' {
  \cadenzaOn
  g4( f) g( a)
  g8( c8)
  c8( g8)
}

\relative c''' {
  \cadenzaOn
  g4.( f) g( a)
  g8( c8.)
  c8.( g8)
}
