
\version "2.1.7"

\header { texidoc="@cindex Staff Lines
Set staff symbol property. "
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
    \new Staff \with { StaffSymbol \set #'line-count = #4 } <<
      \clef bass
      \lower
    >>  
  >>
  \paper { raggedright=##t}  
}
