\version "2.1.28"
\header {
    texidoc = "In lyrics, hyphens may be used."
}
\score{
	<<
	\context Staff \notes { c' (c') c'( c') }
	\context Lyrics \lyrics { bla -- alb xxx -- yyy }
	>>
}






