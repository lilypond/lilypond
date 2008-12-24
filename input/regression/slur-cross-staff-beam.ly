\version "2.12.0"

\header {
  texidoc = "Slurs that depend on a cross-staff beam are not calculated until after line-breaking."
}

\paper{ ragged-right=##t }
\score {
    \new PianoStaff <<
    \context Staff = rh \relative { c'8([ d) \change Staff = lh c,] }
    \context Staff = lh { s4. }
    >>
}