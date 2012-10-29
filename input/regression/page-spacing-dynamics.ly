\version "2.17.6"

\header {
  texidoc = "Dynamic centering still works with alignment-distances."
}

\score {
  \new PianoStaff <<
    \new Staff c'
    \new Dynamics { s2\fff\> s4 s\!\pp }
    \new Staff c'
  >>
  \layout {
    \context {
      \Score
      \override NonMusicalPaperColumn.line-break-system-details =
        #'((alignment-distances . (20)))
    }
  }
}
