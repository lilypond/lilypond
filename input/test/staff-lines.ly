\version "1.5.68"
\header {
texidoc="staff symbol property set workaround"
}


upper = \notes\relative c'' {
  c1-"x" d-"x" e-"x" f-"x"
}

lower = \notes\relative c {
  c1-"x" b-"x" a-"x" g-"x"
}

\score {
  \context PianoStaff <
    %\time 4/4
    \context Staff = upper <
      \upper
      \outputproperty #(make-type-checker 'staff-symbol-interface)
        #'line-count = #5
    >  
    \context Staff = lower <
      \clef bass
      \lower
      \outputproperty #(make-type-checker 'staff-symbol-interface)
        #'line-count = #4
    >  
  >
  \paper { }  
  \midi { }  
}