\version "1.5.68"

\header {
    texidoc = "Note heads are flipped on the stem to prevent collisions.
It also works for whole heads that have invisible stems.
"

}

\score { \notes \relative c''
	 \context Thread {
	     <g4 a c>
	     <c d g a>
	     <c d e >
	     <c c g>
	     <c1 d f g>
    }}
