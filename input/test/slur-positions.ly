\version "1.7.18"
% if this test isn't covered in regression, we're in trouble.  :)  delete.  -gp

\score{
	\notes\relative c''{
		\slurUp
		a2 (a-)\break
		\slurDown
		a2 (a-)\break
		\slurUp
		c2 (c-)\break
		\slurDown
		c2 (c-)\break

	}
	\paper{
		raggedright = ##t
	}
}

%% new-chords-done %%
