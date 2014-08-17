\version "2.19.12"

\header {
  texidoc = "An empty Dynamics context does not confuse the spacing."
}

\score {
  \new PianoStaff <<
    \new Dynamics { s1}
    \new Dynamics { s2\f\> s4 s\!\p }
    \new Staff {d'2 g''}
    \new Dynamics { s1 }
    \new Dynamics { s2\f\> s4 s\!\p }
    \new Dynamics { s1 }
    \new Staff {d'2 g''}
    \new Dynamics { s1 }
    \new Dynamics { s2\sustainOn s4 s\sustainOff }
  >>
  \layout {
    \context {
      \Dynamics
      \override VerticalAxisGroup.nonstaff-relatedstaff-spacing
        = #'((minimum-distance . 5))
    }
  }
}
