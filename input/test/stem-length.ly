\version "1.7.18"

\header{ texidoc = "@cindex Stem Length
You can alter the length of stems. "
}

\score { 
  \context Voice \notes\relative c {
	g''4 \property Voice.Stem \set #'length = #14  g4
	\property Voice.Stem \set #'length = #3 g4  g,,4  
  }
  \paper { raggedright = ##t }  
}
%% new-chords-done %%
