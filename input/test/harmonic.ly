\header {
    texidoc =" Harmonic notes: a different style on the same stem. "
}
\score {
\notes { c'4

	 < c'4 \context Thread = tb {
	     \property Thread.NoteHead \set #'style = #'mensural
	     g''4
	     } >
	     
    }
}
