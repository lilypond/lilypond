\version "1.7.18"
% possible rename to grace-fake.ly   -gp
\header{ texidoc="@cindex Grace Fake
If desired, you can fake a grace note. by changing the timing and fontsize
instead of using \grace. " }
% although I don't know why you'd want to.

\score { 
  \context Voice \notes\relative c {
    % to see the magic: uncomment size stuff in init/paper20.ly
	
	c'4 c4
	
	\property Voice.fontSize= #-2
	b16*1/2  (
	\property Voice.fontSize= #0
	
	) g4 *31/32
	
	a a g2
	
  }
  \paper { }  
  \midi { }
}
%% new-chords-done %%
