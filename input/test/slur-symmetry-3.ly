\version "1.7.16"

\header{
title="symmetry.

Both slurs should look the same.

"
}

\score{
	\notes\relative c'{
		[g'8( e  c'-) g,] r2
		\break
		[d''8( f  a,-) d'] r2
	}
	\paper{
	raggedright = ##t
	}
}
%% new-chords-done %%
