
\version "1.9.4"

\header { texidoc="@cindex Staff Lines
Staff symbol property set workaround. "
}


upper = \notes\relative c'' {
  c1-"x" d-"x" e-"x" f-"x"
}

lower = \notes\relative c {
  c1-"x" b-"x" a-"x" g-"x"
}

\score {
  \context PianoStaff <<
    %\time 4/4
    \new Staff <<
      \upper
      \outputproperty #(make-type-checker 'staff-symbol-interface)
        #'line-count = #5
    >>  
    \new Staff <<
      \clef bass
      \lower
      \outputproperty #(make-type-checker 'staff-symbol-interface)
        #'line-count = #4
    >>  
  >>
  \paper { raggedright=##t}  
}
