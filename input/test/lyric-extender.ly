\version "1.7.18"
% regression -gp
\header { texidoc= "DELETE or REGRESSION. "}
\score{
	<
	\context Staff \notes { c (c-) (c-) c }
	\context Lyrics \lyrics { bla __ alb xxx __ yyy }
	>
}


%% new-chords-done %%
