\version "1.7.7"
\header{
    texidoc = "Additional tricks for dynamics.  Pi`u forte dynamic script"
}

piuf =	\markup {  \italic "pi\\`u" \dynamic "f" }

\score{
    \notes\relative c''{
	c-\piuf
	c
	c2\< \! c2
	
	c2\< \! c2
	}
    }

%% new-chords-done %%
