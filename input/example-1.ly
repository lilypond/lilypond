% A simple scale in LilyPond
%
% Type:
%
%     ly2dvi example-1
%     xdvi example-1     # or your dvi viewer here
%

\score { 
  \context Voice \notes\relative c {
    c' d e f g a b c
  }
  \paper {  }  
  \midi { }
}
