\version "1.9.0"
\header {
    texidoc ="@cindex Artificial Harmonics
For stringed instruments, artificial harmonics are notated with
two different notehead styles on the same stem. " }
\score {
\notes { c'4

	 < c'4 \context Thread = tb {
	     \property Thread.NoteHead \set #'style = #'harmonic
	     g'4
	     } >
	     
    }
	\paper {raggedright=##t}
}

