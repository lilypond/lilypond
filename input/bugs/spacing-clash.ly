\header {
    texidoc="Flat clashes with change-clef.  After real word example."
}
\score {
    <
	\time 3/4
	\context Staff=a \notes\relative c'' {
	    
	    a16 a a a a a a a 
	    a16 a a a
	}
	\context Staff=b \notes\relative c' {
	    \clef bass a16 a a a b8 b \clef violin ces4
	}
    >
    \paper{
	linewidth= -1.
    }
}