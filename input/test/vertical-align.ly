
\version "1.3.110";

\score {
\notes <
	\context GrandStaff < {
	   \property GrandStaff . maxVerticalAlign = #12.0
	   \property GrandStaff . minVerticalAlign = #7.0

	   c'1 \break  c'''''1 
	}
	    { c'1 \break c,,,,1}
	>

>

\paper{}

}
