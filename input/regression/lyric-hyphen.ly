\version "2.2.0"
\header {
    texidoc = "In lyrics, hyphens may be used."
}
\score{
	<<
	\context Staff \notes { c' (c') c'( c') }
	\context Lyrics \lyrics { bla -- alb xxx -- yyy }
	>>
}






