\version "1.0.7";

tie = \notes\transpose c''{

	e4 ~ e e e ~ |
	e ~ a a a ~ |
	a d d d ~ |
	e e e e |
	g,, g,, g,, g,, ~ |
	g,, g,, g,, g,, |
}

\score{
	\tie
	\paper{ 
	      indent = 0.0\pt;
		linewidth= 30.\mm;
		castingalgorithm = \Wordwrap;
	}
}
