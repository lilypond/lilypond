\version "1.7.18"
\header {texidoc="Tests lyric hyphens. "}
\score{
	<
	\context Staff \notes { c' (c'-) (c'-) c' }
	\context Lyrics \context LyricsVoice \lyrics { bla -- alb xxx -- yyy }
	>
}





%% new-chords-done %%
