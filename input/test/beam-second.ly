\version "1.7.18"
% delete me?
\header{
  texidoc="

TODO: what's this for?
manual beaming?  (the two pairs of 16ths)
"
}
\score{
    \notes\relative c''{
	\stemUp
	 b8-[ c]
	 b16-[ c]
	 a'-[ b]
    }
    \paper{
	raggedright = ##t
    }
}%% new-chords-done %%
