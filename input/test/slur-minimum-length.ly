
\version "2.1.22"

\header { texidoc = "@cindex Slur Minimum Length
You can set the minimum length of a slur. " 
}

\score{
	\notes\relative c''{
		\time 2/4
		\override Slur  #'minimum-length = #40
		c(c)
		c~c\break
		}
}

