\version "2.1.10"
\header
{
    texidoc = "Melismata are triggered by manual beams." 
}


\score {
    <<
	\new Staff
	    \notes \relative c'' {
		\property Staff.autoBeaming = ##f
		c8 c8[ c8 c8]  c8    }
	
	\lyricsto "" \new LyricsVoice \lyrics { bla bla bla }
    >>
    \paper { raggedright = ##t }
}
