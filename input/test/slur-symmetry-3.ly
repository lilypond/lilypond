\version "1.7.18"
% regression, and should be merged with -2.
\header { texidoc = "REGRESSION or DELETE. "}
%\header{
%title="symmetry.
%
%Both slurs should look the same. "
%}

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
