\version "2.3.17"
\header {texidoc = "@cindex Slur, dotted
The appearance of slurs may be changed from solid to dotted or dashed.
"
} 
\score{
	\relative c'{
		c( d e  c) |
		\slurDotted
		c( d e  c) |
		\slurSolid
		c( d e  c) |
		\override Slur  #'dashed = #0.0
		c( d e  c) |
		\slurSolid
		c( d e  c) |
	}
	\paper{ raggedright=##t }
}



