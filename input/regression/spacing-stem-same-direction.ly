
\version "2.8.0"
\header {
  texidoc = "For juxtaposed chords with the same direction, a
slight optical correction is used. It is constant, and works only if
two chords have no common head-positions range."
}

\layout { ragged-right = ##t}

\relative c'' {
  \stemDown
  \time 12/4
  f8[
    e f e]  f[ d f d]  f[ c f c]  f[ b, f' b,]  f'[ a, f' a,]
}



