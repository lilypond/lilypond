
\version "1.3.59";

\score {
\notes <
	\context GrandStaff < {
	   \property GrandStaff . maxVerticalAlign = #60.0
	   \property GrandStaff . minVerticalAlign = #35.0

	   c'1 \break  c'''''1 
	}
	    { c'1 \break c,,,,1}
	>

>

\paper{}

}
