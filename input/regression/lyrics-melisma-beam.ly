\version "2.1.26"
\header
{
    texidoc = "Melismata are triggered by manual beams." 
}


\score {
    <<
	\new Staff
	    \notes \relative c'' {
		\set Staff.autoBeaming = ##f
		c8 c8[ c8 c8]  c8    }
	
	\lyricsto "" \new Lyrics \lyrics { bla bla bla }
    >>
    \paper { raggedright = ##t }
}
