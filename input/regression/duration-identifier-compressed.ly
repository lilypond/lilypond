\version "2.13.18"

\header {
  texidoc = "The compression factor of a duration identifier is
correctly accounted for by the parser."
}

% looks like a whole note, has duration of half note
wholeHalved = #(ly:make-duration 0 0 1 2)


\displayMusic \relative c' {
  c\wholeHalved c |
  c\wholeHalved. c4 |
}
