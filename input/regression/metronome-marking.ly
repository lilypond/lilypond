
\header {

    texidoc = "@code{\tempo} directives are printed as metronome markings.

THe marking is left aligned with the time signature, if there is one.
"
    
    }

\version "1.7.18"

\score { \notes \relative c'' {   \tempo \breve = 100 c1 c1 \tempo 8.. = 50 c1 }
\paper {  raggedright = ##t }

     }
	 
