#(ly:set-option 'old-relative)
\version "1.9.0"
% Some beamed and slurred notes of different taste in LilyPond
%
% Type:
%
%     ly2dvi example-2
%     xdvi example-2     # or your dvi viewer here
%

%%
%% For learning LilyPond, please read the tutorial, included in the
%% user-manual.
%% 

\score { 
  \context Voice \notes\relative c {
    a''2 ~ c4( [e8 )e] [a,16 a a a]
  }
  \paper {  }  
  \midi { }
}
