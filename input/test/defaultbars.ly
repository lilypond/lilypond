\version "1.7.16"


\header {

    texidoc = "By setting barAlways and defaultBarType, you can
    automatically insert barlines everywhere." 
}


\score {
	\notes {
		\property Score.barAlways = ##t
		\property Score.defaultBarType = ":|:"
		c4 c4 c4 c4 }
}
%% new-chords-done %%
