
\version "1.0.19";

\score {
\notes <
	\context GrandStaff < {
	   \property GrandStaff . maxVerticalAlign = "60."
	   \property GrandStaff . minVerticalAlign = "35."

	   c'1 \break  c'''''1 
	}
	    { c'1 \break c,,,,1}
	>

>

\paper{}

}
