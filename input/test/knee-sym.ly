\version "1.7.18"
%  candidate for regression.  -gp
\header { texidoc = "REGRESSION or DELETE " }
\score{
    \notes\relative c'{
	 a8-[ b'' a,, b'']
	 b8-[ a,, b'' a,,]
	\stemUp [ b8 \stemDown b'']
    }
    \paper{
	raggedright = ##t 
    }
}%% new-chords-done %%
