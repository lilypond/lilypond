#(ly:set-option 'old-relative)
\version "1.9.0"

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
  \context PianoStaff <
    %\time 4/4
    \context Staff = upper \upper
    \context Staff = lower <
      \clef bass
      \lower
    >  
  >
  \paper { }  
  \midi { }  
}
