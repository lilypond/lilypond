\version "2.1.21"
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
	
	\lyricsto "" \new Lyrics \lyrics { bla bla bla }
    >>
    \paper { raggedright = ##t }
}
