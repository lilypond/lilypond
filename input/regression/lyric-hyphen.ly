\version "2.1.19"
\header {texidoc="Tests lyric hyphens. "}
\score{
	<<
	\context Staff \notes { c' (c') (c') c' }
	\context LyricsVoice \lyrics { bla -- alb xxx -- yyy }
	>>
}






