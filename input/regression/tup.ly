\version "1.3.146"
\header{
    
texidoc="
Tuplets are indicated by a bracket with a number.  There should be no
bracket if there is a beam exactly  matching  the length of the tuplet.
The bracket does not interfere with the stafflines, and the number is
centered in the gap in the bracket.

The bracket stops at the end of the stems, if the stems have the same
direction as the


"

}

\score{
	\notes \context Voice \relative c'' {
		 \times 2/3 { \times 2/3 { a8 b c}  c }
		 \times 2/3 { r8 [b f] }
		 \times 2/3 { r8 b r8 }
		 c4 |
		 
		 \times 3/4 { c4 c4 c4 c4 } c4 | 
		 
		 \time 6/8
		 \times 6/9 { c8 c c c c c c c c }

		 }
}


