\header {

    texidoc = "By setting texts starting with a multi-measure rest, an 
extra spacing column is created. This should not cause problems."
    }
\version "2.2.0"

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
