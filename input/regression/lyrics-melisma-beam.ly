\version "2.3.17"
\header
{
    texidoc = "Melismata are triggered by manual beams." 
}


\score {
    <<
	\new Staff
	     \relative c'' {
		\set Staff.autoBeaming = ##f
		c8 c8[ c8 c8]  c8    }
	
	\lyricsto "" \new Lyrics \lyricmode { bla bla bla }
    >>
    \paper { raggedright = ##t }
}
