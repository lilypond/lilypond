\version "2.2.0"

\header {
  texidoc = "Simple piano music." 
}

upper = \notes\relative c'' {
  a b c d
}

lower = \notes\relative c {
  a2 c
}

\score {
  \context PianoStaff <<
    \set PianoStaff.instrument = "Piano  " % set instrument name.
    \context Staff = upper \upper
    \context Staff = lower <<
      \clef bass
      \lower
    >>  
  >>
  \paper { }  
  \midi { }  
}
