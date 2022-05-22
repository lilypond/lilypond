\version "2.23.9"

\header {
  texidoc = "@code{\\partial} can be can be called in mid-piece in
multiple contexts."
}


melodyOne = \relative {
  \time 6/8
  a'8 a a a a a |
  \partial 8
  d8 |
  c8 c c c c c |
}

chordsOne = \chordmode {
  \time 6/8
  a2. |
  \partial 8
  s8 |
  a2. |
}

\score {
  <<
    \set Score.barNumberVisibility = #all-bar-numbers-visible
    \override Score.BarNumber.break-visibility = #all-visible
    \new ChordNames { \chordsOne }
    \new Staff { \melodyOne }
  >>
}
