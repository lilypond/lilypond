\header {

    texidoc = "By setting texts starting with a mmrest we create
an extra spacing column. This should not cause problems."
    }
\version "2.1.22"

\score {
    <<
	\set Score.skipBars = ##t
	\context Staff = flute \notes \new Voice { 
	    <<  { R1*40 }  { s1*0^"bla" }>> 
	}
    >>
    \paper {
	raggedright = ##t
    }
 }
