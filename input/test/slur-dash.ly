\version "1.9.8"
\header {texidoc = "@cindex Slur, dotted
You can print different kinds of slurs (dotted, dashed, etc). "
} 
\score{
	\notes{
		c( d e  c) |
		\slurDotted
		c( d e  c) |
		\slurSolid
		c( d e  c) |
		\property Voice. Slur \set #'dashed = #0.0
		c( d e  c) |
		\slurSolid
		c( d e  c) |
	}
	\paper{ raggedright=##t }
%	      indent = 0.0\pt
		%for broken!
		% linewidth= 30.\mm
%	}
}



