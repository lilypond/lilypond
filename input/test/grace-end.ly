\version "1.7.18"
% I don't see the point.  I'll add this to my next round
% of delete'ing, unless Han notices this comment in the
% changelog and deletes it before I ask.  :)   -gp
\header {texidoc = "DELETE ME " }

%\header { texidoc="@cindex Grace End
%You can put grace notes after a note. " }

\score { 
  \context Voice \notes\relative c {
    
	c4 \grace {  d16-[ d16] }
	
  }
	\paper {
		raggedright = ##t
	}  
  \midi { }
}
%% new-chords-done %%
