
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
    >>  
    \new Staff <<
      \clef bass
      \lower
      \applyoutput #(outputproperty-compatibility (make-type-checker 'staff-symbol-interface)
        'line-count = 4)
    >>  
  >>
  \paper { raggedright=##t}  
}
