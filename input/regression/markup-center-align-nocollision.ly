\version "2.16.0"

\header {
  texidoc = "Text markup using @code{center-column} shall
still reserve space for its whole width and not overwrite the previous stencil."
}

% Here the two center-columns should not collide with the +
\markup \line {
  \center-column { \line {XXX} \line {Y} }
  "+"
  \center-column { \line {XXX} \line {Y}}
}
