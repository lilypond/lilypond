\header {

texidoc ="tuplet bracket doesn't behave properly at line break. Not a real problem.";
}

\score{
	\notes\relative c''< 
	{	% broken cresc dumps core
		c2 c4*2/3 \times 2/3 { c( c c }
		c4 c c c
	} >
	\paper{
	indent = 0.;
		linewidth=30.\mm;
	}
}
