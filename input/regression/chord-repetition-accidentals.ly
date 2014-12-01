\version "2.18.0"

\header {
  texidoc = "Chord repeats should omit forced and reminder accidentals."
}

\relative
{
  <f'! a d f!> q q q |
  <f? a d f?> q q q
}
