\version "1.7.18"

\header{
texidoc =	 "Test beam quant positions for horizontal beams."
}

\score{
	\notes\relative c' { 
		 c8-[ c]  a''-[ a]
		 a,-[ a]  c-[ c]
		 d,8-[ d]  g'-[ g]
		 g,-[ g]  d'-[ d]
		 c,16-[ c c c]  a''-[ a a a]
		 a,-[ a a a]  c-[ c c c]
		\break
		 c,32-[ c c c c c c c]  a''-[ a a a a a a a]
		 f,-[ f f f f f f f]  e'-[ e e e e e e e]
		\break
	}
}

%% new-chords-done %%
