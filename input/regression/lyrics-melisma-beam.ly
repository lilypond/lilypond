#(ly:set-option 'old-relative)
\version "1.9.0"
\header
{
    texidoc = "Melismata are triggered by manual beams." 
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
