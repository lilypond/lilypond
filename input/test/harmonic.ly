\version "2.3.4"
\header {
    texidoc ="@cindex Artificial Harmonics

Artificial harmonics are notated with a different notehead style, by 
marking the harmonic pitch with @code{\harmonic}."

}

\score {
     { 
	     <c' g'\harmonic>4
	 }
    \paper {
	raggedright=##t
    }
}

