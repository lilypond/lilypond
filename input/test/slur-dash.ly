\version "1.5.68"


\score{
	\notes{
		c( d e )c |
		\slurDotted
		c( d e )c |
		\slurSolid
		c( d e )c |
		\property Voice. Slur \set #'dashed = #0.0
		c( d e )c |
		\slurSolid
		c( d e )c |
	}
	\paper{ 
	      indent = 0.0\pt
		%for broken!
		% linewidth= 30.\mm

	}
}


