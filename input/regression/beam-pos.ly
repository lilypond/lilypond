\version "1.7.18"
%  does this belong in regression?  delete it?  it doens't look
%  like a special trick or a great learning tool, but it might
%  be useful in testing length os beam stems, or something like
%  that.

\header{
texidoc =	 "test beam quant positions"
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
		 c,8-[ d]  a''-[ g]
		 g,-[ f]  d'-[ e]
		\break
	}
	\paper{
	}
}

%% new-chords-done %%
