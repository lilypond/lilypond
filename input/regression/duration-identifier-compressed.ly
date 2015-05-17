\version "2.19.21"

\header {
  texidoc = "The compression factor of a duration identifier is
correctly accounted for by the parser."
}

% looks like a whole note, has duration of half note
wholeHalved = #(ly:make-duration 0 0 1/2)


\relative {
  c'\wholeHalved c |
  c\wholeHalved. c4 |
}
