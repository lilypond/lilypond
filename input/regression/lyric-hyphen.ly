\version "2.1.22"
\header {texidoc="Tests lyric hyphens. "}
\score{
	<<
	\context Staff \notes { c' (c') (c') c' }
	\context Lyrics \lyrics { bla -- alb xxx -- yyy }
	>>
}






