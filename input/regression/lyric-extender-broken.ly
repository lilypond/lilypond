\version "2.1.19"
\header
{

    texidoc = "Lyric extenders run to the end of the line if it
continues the next line. Otherwise, it should run to the last note
of the melisma."

}

\score {
    << 
	\context Voice=A \notes {
	    a1 ( a1 \break
	    a) a2( b) \break
	    a2 
	}
	\lyricsto A \context LyricsVoice \lyrics { a __ a __ ha }
    >>
    \paper {
	raggedright = ##t
    }
}
