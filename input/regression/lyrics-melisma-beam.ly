
\version "1.7.17"
\header
{
    texidoc = "melismata are triggered by manual beams." 
}


\score {
        \simultaneous {
          \addlyrics
             \context Staff = mel
             \notes \relative c'' {
		 \property Staff.autoBeaming = ##f
		 c8 c8-[ c8 c8-]  c8    }
          	
             \context Lyrics \lyrics { bla bla bla }
        }
        \paper { raggedright = ##t }
}
