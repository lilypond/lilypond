% Some beamed and slurred notes of different taste in LilyPond
%
% Type:
%
%     ly2dvi example-2
%     xdvi example-2     # or your dvi viewer here
%

\score { 
  \context Voice \notes\relative c {
    a''2 ~ c4( [e8 )e] [a,16 a a a]
  }
  \paper {  }  
  \midi { }
}
