\version "2.17.11"

\header {
  texidoc = "The compression factor of a duration identifier is
correctly accounted for by the parser."
}

% looks like a whole note, has duration of half note
wholeHalved = #(ly:make-duration 0 0 1/2)


\relative c' {
  c\wholeHalved c |
  c\wholeHalved. c4 |
}
