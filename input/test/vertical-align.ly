
\version "1.3.93";

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
