
\version "2.4.0"
\header {
    texidoc = "Grace notes may be put in a @code{partcombine}r."
}

\layout { raggedright= ##t }

\score {
    \new Staff
	    \partcombine 
	      \relative c'' {
		c4 d e f  \grace f16 g1
	    }
	      \relative c' {
		c4 d e2  g1
	    }
	
    
}

