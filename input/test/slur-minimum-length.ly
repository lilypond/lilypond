
\version "1.9.1"

\header { texidoc = "@cindex Slur Minimum Length
You can set the minimum length of a slur. " 
}

\score{
	\notes\relative c''{
		\time 2/4
		\property Voice.Slur \set #'minimum-length = #40
		c(c)
		c~c\break
		}
}

