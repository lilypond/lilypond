shortlong = \melodic{
	c4()c( c c  |
	c c c c |
	c c c c |
	\break
	c c c )c |
	c c c c |
	c c c c |
	c c c c |
	c c c c |
}

\score{
	\shortlong
	\paper{ 
	      indent = 0.0\pt;
		% linewidth= 30.\mm;
		castingalgorithm = \Wordwrap;
	}
}
