\version "1.7.18"
% possible rename to bar-something.

\header{ texidoc = "@cindex Bar Length
You can alter the length of bars in a number of ways. "
}

\score { 
  \context Voice \notes\relative c {
    
	
	% \property Score. measurePosition = #(ly:make-moment -1 4)
	\partial 4
	c''4 c4 c4 c2 c1
	\cadenzaOn  c8-[ d e f]  g-[ a b c b c b c]
	\cadenzaOff
	c4 c4 c4 c4
	\property Score. measureLength = #(ly:make-moment 5 4)
	
	c1 c4
	c1 c4 
	c4 c4
	\property Score. measurePosition = #(ly:make-moment -3 8)
	b8 b b
	c4 c1
	
  }
  \paper { raggedright= ##t}  
}
%% new-chords-done %%
