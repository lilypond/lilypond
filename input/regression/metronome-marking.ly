

\header {

    texidoc = "Here @code{\\tempo} directives are printed as metronome markings.

The marking is left aligned with the time signature, if there is one.
"
    
    }

\version "2.4.0"

\score {  \relative c'' {   \tempo \breve = 100 c1 c1 \tempo 8.. = 50 c1 }
\layout {  raggedright = ##t }

     }
	 
