
\version "2.3.22"

\header {
    texidoc = "Note heads are flipped on the stem to prevent collisions.
It also works for whole heads that have invisible stems.
"

}

    \layout { raggedright= ##t }

\score {  \relative c''
	 \context Voice {
	     <g a c>4
	     <c d g a>
	     <c d e >
	     <c c g>
	     <c d f g>1
    }}

