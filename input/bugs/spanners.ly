% spanners start at line start
\score{
	\notes\relative c''{
		% broken cresc dumps core
		c2\< c4 \times 2/3 { c( c c }
		c4 c )\!c c
	}
	\paper{
		linewidth=30.\mm;
	}
}
