
\version "2.1.26"
\header {
    texidoc = "Grace notes may be put in a @code{partcombine}r."
}

\paper { raggedright= ##t }

\score {
    \new Staff
	    \partcombine 
	     \notes \relative c'' {
		c4 d e f  \grace f16 g1
	    }
	     \notes \relative c' {
		c4 d e2  g1
	    }
	
    
}

