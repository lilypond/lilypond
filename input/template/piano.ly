\version "2.3.4"

\header {
  texidoc = "Simple piano music." 
}

upper = \relative c'' {
  a b c d
}

lower = \relative c {
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
