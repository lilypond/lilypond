
\version "2.1.7"

\header {
    texidoc = "Note heads are flipped on the stem to prevent collisions.
It also works for whole heads that have invisible stems.
"

}

    \paper { raggedright= ##t }

\score { \notes \relative c''
	 \context Thread {
	     <g a c>4
	     <c d g a>
	     <c d e >
	     <c c g>
	     <c d f g>1
    }}

