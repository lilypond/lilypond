\version "2.19.21"

\header {
  texidoc = "Slurs that depend on a cross-staff beam are not calculated until
after line-breaking, and after inside-going articulations have been placed."
}

\paper { ragged-right=##t }
\score {
    \new PianoStaff <<
    \context Staff = rh \relative { c''8([ d) \change Staff = lh c,] r
    \stemDown f'8 \change Staff = rh f_( \change Staff = lh f_\marcato f)}
    \context Staff = lh { s1 }
    >>
}
