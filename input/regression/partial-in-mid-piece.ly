\version "2.25.22"

\header {
  texidoc = "@code{\\partial} can be can be called in mid piece in multiple
contexts.  Measure@tie{}2 should contain two beamed eighth notes."
}

melodyOne = \relative {
  \time 2/4
  a'8 a a a |
  \partial 4
  d8 d |
  c8 c c c |
}

chordsOne = \chordmode {
  \time 2/4
  a2 |
  \partial 4
  s4 |
  a2 |
}

\score {
  <<
    \set Score.barNumberVisibility = #all-bar-numbers-visible
    \override Score.BarNumber.break-visibility = #all-visible
    \overrideTimeSignatureSettings 2/4 #1/8 1,1,2 #'()
    \new ChordNames { \chordsOne }
    \new Staff { \melodyOne }
  >>
}
