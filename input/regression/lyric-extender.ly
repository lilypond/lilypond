\version "1.7.18"
\header { texidoc= "Tests lyric extenders. "}
\score{
	<
	\context Staff \notes { c (c-) (c-) c }
	\context Lyrics \lyrics { bla __ alb xxx __ yyy }
	>
}


%% new-chords-done %%
