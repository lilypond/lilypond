\version "2.3.16"


\header { texidoc= "

Whenever a voice switches to another staff a line connecting the notes
can be printed automatically. This is enabled if the property
@code{followVoice} is set to true. "

      }
\score {
     \relative c'
    \context PianoStaff <<
	\set PianoStaff.followVoice = ##t
	\context Staff = one \context Voice {
	    c1
	    \change Staff=two
	    b2 a
	}
	\context Staff = two {\clef bass \skip 1*2 }
    >>
    \paper{ 
	raggedright = ##t 
    }
}
