
\version "2.1.26"

\header {

    texidoc="@cindex Staff Lines Staff symbol properties may also be
 set with @code{\\property}. "
	  
}


upper = \notes\relative c'' {
  c1 d e f
}

lower = \notes\relative c {
  c1 b a g
}

\score {
  \context PianoStaff <<
    \new Staff <<
      \upper
    >>  
    \new Staff  {
	\override Staff.StaffSymbol  #'line-count = #4 
        \clef bass
        \lower
    }
  >>
  \paper { raggedright=##t}  
}
