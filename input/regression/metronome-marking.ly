

\header {

    texidoc = "Here @code{\\tempo} directives are printed as metronome markings.

The marking is left aligned with the time signature, if there is one.
"
    
    }

\version "2.3.4"

\score {  \relative c'' {   \tempo \breve = 100 c1 c1 \tempo 8.. = 50 c1 }
\paper {  raggedright = ##t }

     }
	 
