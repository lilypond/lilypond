\version "2.3.22"
% Some beamed and slurred notes of different taste in LilyPond
%
% Type:
%
%     lilypond example-2
%     xdvi example-2     # or your dvi viewer here
%

%%
%% For learning LilyPond, please read the tutorial, included in the
%% user-manual.
%% 

\score { 
  \context Voice \relative c {
    a''2 ~ a4( e8[ )e] a,16[ a a a]
  }
  \layout {  }  
  \midi { }
}
