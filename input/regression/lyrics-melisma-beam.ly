\version "2.1.7"
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
	
	\newaddlyrics "" \new LyricsVoice \lyrics { bla bla bla }
    >>
    \paper { raggedright = ##t }
}
