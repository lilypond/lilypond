\version "1.7.18"
\header { texidoc= "Tests lyric extenders. "}
\score{
\notes \relative c'	<
	\context Staff { c (c-) (c-) c }
	\context Lyrics \lyrics { bla __ alb xxx __ yyy }
	>
}



