\version "1.7.18"
\header {
    texidoc ="@cindex Artificial Harmonics
For stringed instruments, artificial harmonics are notated with
two different notehead styles on the same stem. " }
\score {
\notes { c'4

	 < c'4 \context Thread = tb {
	     \property Thread.NoteHead \set #'style = #'mensural
	     g'4
	     } >
	     
    }
	\paper {raggedright=##t}
}
%% new-chords-done %%
