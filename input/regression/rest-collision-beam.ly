
\header {
    
    texidoc = "Rests under beams are only moved if necessary."

}

\version "2.1.22"

\score {
    \new Staff
    \notes {
	\stemUp
	\transpose c c' {
	    c''8[ r8 c''8 c''8]
	    c8[ r8 c8 c8]
	    c8[ r8 r8 c'''8]	
	    \stemDown
	    c8[ r8 c8 c8]
	    c''8[ r8 c''8 c''8]
	    c'8[ r8 r8 c'''8]
	}
    }
    \paper { raggedright = ##t }
}
