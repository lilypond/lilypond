\header{
texidoc="
Tuplets are indicated by a bracket with a number.  There should be no
bracket if there is one beam that matches  the length of the tuplet.
The bracket does not interfere with the stafflines, and the number is
centered in the gap in the bracket.
";
}
\score{
	\notes \context Voice \relative c'' {
		 \times 2/3 { \times 2/3 { a8 b c}  c }
		 \times 3/4 { c4 c4 c4 c4 }
		 \time 6/8;
		 \times 6/9 { c8 c c c c c c c c }

		 }
}

\version "1.3.117"; 
