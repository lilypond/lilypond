\version "2.3.4"

\header { texidoc= "In lyric extenders, a syllable may be extended over several notes. "}

\paper { raggedright= ##t }
\score{
 \relative c'	<<
	\context Voice = melody {
	    c8[ ( d] )
	    r4 f4 }
	\lyricsto melody \context Lyrics \lyrics { ah __ ha  }
	>>
}




