\version "1.3.93";

\score{
	\notes{
		c( d e )c |
		\slurdotted
		c( d e )c |
		\slurnormal
		c( d e )c |
		\property Voice.slurDash = #0.0
		c( d e )c |
		\slurnormal
		c( d e )c |
	}
	\paper{ 
	      indent = 0.0\pt;
		%for broken!
		% linewidth= 30.\mm;

	}
}


