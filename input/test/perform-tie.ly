\version "1.7.18"
% regression or delete. -gp
\header { texidoc= "DELETE or REGRESSION. " }

\score { 
  \context Voice \notes\relative c {
    c8 c c ~ c c c c ~ c ~ c ~ c c c
	%\grace { d16 c16 d16 } c8
	
  }
  \paper { }  
  \midi { }
}
%% new-chords-done %%
