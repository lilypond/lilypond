
\version "2.1.36"

\header {

    texidoc="@cindex Staff Lines 
The number of lines in a staff may changed by overriding @code{line-count}
in the properties of @code{StaffSymbol}.
"
	  
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
