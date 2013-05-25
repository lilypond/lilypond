\version "2.16.0"

\header {
  texidoc = "Slurs that depend on a cross-staff beam are not calculated until
after line-breaking and after inside-going articulations have been placed."
}

\paper { ragged-right=##t }
\score {
    \new PianoStaff <<
    \context Staff = rh \relative c'' { c8([ d) \change Staff = lh c,] r
     \stemDown c'8 \change Staff = rh c_( \change Staff = lh c_\marcato c)}
    \context Staff = lh { s1 }
    >>
}
