
\version "2.1.26"

\header {
    
    texidoc = "Stems in forced directions (as well as the ones
    starting from the middle line) are shortened."

}

\score {
    \notes \relative c'' { \stemDown d c b a g f e
			   \stemBoth
			   \stemUp a b c d e f g a 
		       } 
    \paper  {raggedright = ##t }
}
