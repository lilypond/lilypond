\version "1.7.6"
\header{
    texidoc="Second line has bar-numbers on start of every measure."
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
