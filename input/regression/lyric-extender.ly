\version "2.1.26"

\header { texidoc= "In lyric extenders, a syllable may be extended over several notes. "}

    \paper { raggedright= ##t }
\score{
\notes \relative c'	<<
	\context Staff {
	    c8[ ( d] )
	    r4 f4 }
	\context Lyrics \lyrics { xxx __ \skip 4 x  }
	>>
}




