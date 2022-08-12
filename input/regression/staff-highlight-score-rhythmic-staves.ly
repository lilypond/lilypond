\version "2.23.12"

\header {
  texidoc = "This test exercises highlights spanning a set of rhythmic
staves with different font sizes.  At the bottom and at the top, the
highlight should extend as far as the bar lines do."
}


\layout {
  \context {
    \RhythmicStaff
    \remove Staff_highlight_engraver
  }
  \context {
    \Score
    \consists Staff_highlight_engraver
  }
}

<<
  \new RhythmicStaff { \staffHighlight lightsteelblue 4 8~ 4 8 4 }
  \new RhythmicStaff \with { \magnifyStaff #2 } { 4 8~ 4 8 4 }
>>
