\version "1.7.18"
% renamed file to bar-number-show-all.ly from bar-script-visibility.ly
% for greater clarity
\header{
    texidoc="@cindex Bar Number Show All
Second line has bar numbers on start of every measure."
}

\score{
    \notes\relative c'{
	c1 c c
	\property Score.BarNumber \override
	    #'break-visibility = #end-of-line-invisible
	\break
	c c c
    }
}

%% new-chords-done %%
