\version "1.9.8"

\header { texidoc= "Tests lyric extenders. "}

    \paper { raggedright= ##t }
\score{
\notes \relative c'	<<
	\context Staff {
	    c8[ ( d] )
	    r4 f4 }
	\context Lyrics \lyrics { xxx __ \skip 4 x  }
	>>
}




