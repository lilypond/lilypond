\version "1.7.16"

\header {
    texidoc = "Note heads are flipped on the stem to prevent collisions.
It also works for whole heads that have invisible stems.
"

}

\score { \notes \relative c''
	 \context Thread {
	     <<g a c>>4
	     <<c d g a>>
	     <<c d e >>
	     <<c c g>>
	     <<c d f g>>1
    }}
%% new-chords-done %%
