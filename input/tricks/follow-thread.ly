
\header{
texidoc="
Theads can be traced automagically when they switch staffs by setting
property @code{followVoice}.
";
}
% followThread: connect note heads with line when thread switches staff 

fragment = \notes {
  \context PianoStaff <
    \property PianoStaff.followVoice = ##t
    \context Staff \context Voice {
      c'1
      \translator Staff=two
      b2 a
    }
    \context Staff=two {\clef bass; \skip 1*2;}
  >
}

\paper { linewidth = -1.; } 

\score {
  \notes\relative c \fragment
  \paper { }  
}
